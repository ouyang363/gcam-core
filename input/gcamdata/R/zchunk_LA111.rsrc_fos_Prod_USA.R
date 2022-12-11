# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_LA111.rsrc_fos_Prod_USA
#'
#' Calculate historical fossil energy production and fossil resource supply curves for US states.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L111.gas_supply_state_T_EJ}, \code{L111.unconv_gas_supply_state_EJ},
#' \code{L111.gas_prod_state_T_Yh_EJ}. The corresponding file in the
#' original data system was \code{LA111.rsrc_fos_Prod.R} (energy level1).
#' @details Resource types are mapped from USGS basins to the states, and resource grades are defined.
#' “Other” unconventional gas resources are calculated by summing USGS resource data to a national total
#' across resource types, subtracting this total from the energy data system’s natural gas resource curve
#' (L111.RsrcCurves_EJ_R_Ffos.csv) to find remaining natural gas resources, and allocating this remainder
#' to states based on their share of unconventional gas resources (coalbed methane, shale gas, and tight gas).
#' USA-level natural gas production data (from IEA) is then downscaled by state and gas resource type.
#' Conventional gas production for states with no tight gas resources is taken as the remainder of total production
#' less offshore, shale, and coalbed production; for states which do have tight gas resources, conventional and tight
#' gas production is re-assigned.
#' State-level offshore gas production – distinct from the four outer continental shelf (OCS) regions which are federally
#' controlled – is reassigned to conventional production for consistency with USGS resource supply data.
#' Finally, the share of USA-level natural gas production is calculated for each state/ resource type,
#' and then applied to the IEA national production estimates in L111.Prod_EJ_R_F_Yh.csv to ensure that aggregate
#' state-level production will equal the USA-region production from the energy data system for each historical period.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter if_else group_by left_join mutate select summarise
#' @importFrom tidyr complete replace_na
#' @author YO Dec 2022
module_gcamusa_LA111.rsrc_fos_Prod_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/USGS_gas_supply_Quad",
             FILE = "gcam-usa/USGS_basin_state_mapping",
             FILE = "gcam-usa/BOEM_gas_supply_EJ",
             FILE = "gcam-usa/EIA_gas_A_MMcf",
             FILE = "gcam-usa/EIA_gas_offshore_MMcf",
             FILE = "gcam-usa/EIA_gas_shale_MMcf",
             FILE = "gcam-usa/EIA_gas_coalbed_MMcf",
             FILE = "gcam-usa/EIA_gas_US_T_MMcf",
             "L111.Prod_EJ_R_F_Yh",
             "L111.RsrcCurves_EJ_R_Ffos"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L111.gas_supply_state_T_EJ",
             "L111.unconv_gas_supply_state_EJ",
             "L111.gas_prod_state_T_Yh_EJ"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    year.state <- value.state <- year <- value <- type <- type.name <- year.us <- value.us <- region <-
      value.us.earlist.state.year <- us.scaler <- value.state.scaled <- fraction <- state <- resource <- grade.1 <- F95 <-
      grade.2 <- F50 <- grade.3 <- F05 <- total <- region_sum <- grade <- cum_avail <- available <- cum_avail_remain <-
      USGS_total <- share <- extractioncost <- remainder <- offshore <- shale <- coalbed <- conventional <- share.calc <-
      share.diff <- share.comp <- gas.diff <- share.supply <- tg.move <- tight <- eia.prod <- eia.total <- GCAM_region_ID <-
      us.prod <- prod <- depresource <- subresource <- NULL  # silence package check notes

    # Load required inputs
    USGS_gas_supply_Quad <- get_data(all_data, "gcam-usa/USGS_gas_supply_Quad")
    USGS_basin_state_mapping <- get_data(all_data, "gcam-usa/USGS_basin_state_mapping")
    BOEM_gas_supply_EJ <- get_data(all_data, "gcam-usa/BOEM_gas_supply_EJ")
    EIA_gas_A_MMcf <- get_data(all_data, "gcam-usa/EIA_gas_A_MMcf")
    EIA_gas_offshore_MMcf <- get_data(all_data, "gcam-usa/EIA_gas_offshore_MMcf")
    EIA_gas_shale_MMcf <- get_data(all_data, "gcam-usa/EIA_gas_shale_MMcf")
    EIA_gas_coalbed_MMcf <- get_data(all_data, "gcam-usa/EIA_gas_coalbed_MMcf")
    EIA_gas_US_T_MMcf <- get_data(all_data, "gcam-usa/EIA_gas_US_T_MMcf")
    L111.Prod_EJ_R_F_Yh <- get_data(all_data, "L111.Prod_EJ_R_F_Yh", strip_attributes = TRUE)
    L111.RsrcCurves_EJ_R_Ffos <- get_data(all_data, "L111.RsrcCurves_EJ_R_Ffos", strip_attributes = TRUE)

    # ===================================================
    # Perform computations

    # NOTE: THIS FUNCTION STILL NEEDS TO BE RE-WRITTEN IN DPLYR
    # Extend state gas production backwards using the state shares from the earliest
    # available year and scaling by the total US production of that type. Note if the
    # year is before any data that we have then constant production is assumed.
    extend_back_with_us <- function(state.d, us.d, type.name) {

      state.d <- state.d %>% gather_years() %>% rename(year.state = year, value.state = value)
      us.d <- us.d %>% gather_years() %>% filter(type == type.name) %>% rename(year.us = year, value.us = value)

      tb_region_year <- tibble(region = unique(state.d$region)) %>%
        repeat_add_columns(tibble(year = HISTORICAL_YEARS))

      state.d.noNA <- tb_region_year %>%
        # will have NA rows for years with no state-level data available
        left_join(state.d, by = c("year" = "year.state", "region")) %>%
        # will have NA rows for years with no US total data available
        left_join(us.d, by = c("year" = "year.us")) %>%
        group_by(region) %>%
        # copy data to the nearest available years prior or after state/US data available years
        mutate(value.state = approx_fun(year, value.state, rule = 2), year = as.integer(year),
               value.us = approx_fun(year, value.us, rule = 2), year = as.integer(year)) %>%
        # copy the same resource type to all rows
        replace_na(list(type = type.name)) %>%
        ungroup()

      # make adjustment
      state.d.scaled <- state.d.noNA %>%
        as_tibble() %>%
        left_join_error_no_match(state.d.noNA %>%
                                   filter(year == min(state.d$year.state)) %>%
                                   select(region, value.us.earlist.state.year = value.us), by = "region") %>%
        mutate(us.scaler = value.us / value.us.earlist.state.year) %>%
        mutate(value.state.scaled = ifelse(year < min(state.d$year.state),
                                           value.state * us.scaler, value.state)) %>%
        select(region, type, year, value = value.state)

      return(state.d.scaled)
    }

    # Map supply from basins to states.
    L111.gas_prob_names <- names( USGS_gas_supply_Quad )[ grep('^F\\d\\d$', names( USGS_gas_supply_Quad ), perl=TRUE ) ]
    USGS_basin_state_mapping %>%
      # number of rows will change since one basin could be mapped to multiple states
      left_join(USGS_gas_supply_Quad, by = c("basin")) -> L111.gas_supply_state_T_EJ

    # Note the conversion from quadrillion BTU to EJ is the same as BTU to KJ
    L111.gas_supply_state_T_EJ %>%
      gather(type, value, -state, -basin, -fraction, -row, -resource) %>%
      mutate(value = value * fraction * CONV_KBTU_EJ ) %>%
      spread(type, value) %>%
      group_by(state, resource) %>%
      summarise_at(vars(L111.gas_prob_names), sum, na.rm = TRUE) %>%
      ungroup() -> L111.gas_supply_state_T_EJ

    # The supply is given as total cumulative production at a given probability, we
    # need the grades as the additional supply available in that grade.
    # TODO: code the mutate more flexibly (if possible)
    L111.gas_supply_state_T_EJ %>%
      mutate(grade.1 = F95, grade.2 = F50 - F95, grade.3 = F05 - F50) %>%
      select(-one_of(L111.gas_prob_names)) -> L111.gas_supply_state_T_EJ

    L111.gas_supply_state_T_EJ %>% filter(grade.1 > 0 | grade.2 > 0 | grade.3 > 0 ) %>%
      bind_rows(BOEM_gas_supply_EJ %>% mutate(resource = "offshore gas") %>%
                  rename(state = region)) -> L111.gas_supply_state_T_EJ

    # Add in "other" unconventional gas resources
    # Create state shares from USGS data
    L111.gas_supply_state_T_EJ %>% filter(resource %in% c("coalbed methane", "shale gas", "tight gas")) %>%
      mutate(total = grade.1 + grade.2 + grade.3) %>%
      group_by(state) %>%
      summarise(total = sum(total)) %>%
      mutate(region_sum = sum(total)) %>%
      mutate(share = total / region_sum) %>%
      select(-total, -region_sum) -> L111.unconv_gas_supply_downscale

    # Sum all natural gas resources to national total (USGS data). Subtract from energy data system
    # natural gas resource curve (L111.RsrcCurves_EJ_R_Ffos) to get remaining natural gas resources.
    L111.gas_supply_state_T_EJ %>% mutate(GCAM_region_ID = gcamusa.USA_REGION_NUMBER) %>%
      group_by(GCAM_region_ID) %>%
      summarise(USGS_total = sum(grade.1, grade.2, grade.3)) %>%
      ungroup() -> L111.gas_supply_USA_T_EJ

    L111.RsrcCurves_EJ_R_Ffos %>% filter(GCAM_region_ID == gcamusa.USA_REGION_NUMBER) %>%
      filter(resource == "natural gas") %>%
      mutate(grade = as.numeric(gsub("grade", "", grade))) %>%
      mutate(cum_avail = cumsum(available)) %>%
      left_join_error_no_match(L111.gas_supply_USA_T_EJ, by = c("GCAM_region_ID")) %>%
      mutate(cum_avail_remain = cum_avail - USGS_total) %>%
      filter(cum_avail_remain >= 0) %>%
      mutate(available = ifelse(grade == min(grade), cum_avail_remain, available)) %>%
      select(-cum_avail, -USGS_total, -cum_avail_remain) %>%
      mutate(grade = paste("grade", grade, sep = ' ')) -> L111.RsrcCurves_EJ_R_gas

    # Apply state shares to remaining natural gas resources
    # Rename the resource to indicate it is "other" unspecified resource
    unconv_gas_supply_states <- distinct(L111.unconv_gas_supply_downscale %>% select(state) %>% as_tibble())

    L111.RsrcCurves_EJ_R_gas %>%
      as_tibble() %>%
      repeat_add_columns(unconv_gas_supply_states) %>%
      left_join_error_no_match(L111.unconv_gas_supply_downscale, by = c("state")) %>%
      mutate(available = available * share) %>%
      mutate(resource = "unconventional gas other") %>%
      select(state, resource, grade, extractioncost, available) -> L111.unconv_gas_supply_state_EJ

    # Process historical gas production.  We need to downscale IEA US level total
    # natural gas production to split by state and gas resource type. For this we
    # have *some* data: US level production by type and state level production by:
    # total, offshore, shale, and coalbed.  We will need to back out tight gas and
    # conventional production at the state level.

    # Extend state gas production backwards using the state shares from the earliest
    # available year and scaling by the total US production of that type
    L111.EIA_gas_offshore_MMcf <- extend_back_with_us( EIA_gas_offshore_MMcf, EIA_gas_US_T_MMcf, "Lower 48 offshore" )
    L111.EIA_gas_shale_MMcf <- extend_back_with_us( EIA_gas_shale_MMcf, EIA_gas_US_T_MMcf, "Shale Gas" )
    L111.EIA_gas_coalbed_MMcf <- extend_back_with_us( EIA_gas_coalbed_MMcf, EIA_gas_US_T_MMcf, "Coalbed methane" )

    # Combine EIA production data, drop the aggregate US rows for now to avoid having to keep it consistent
    EIA_gas_A_MMcf %>%
      gather_years() %>%
      complete(nesting(region), year = HISTORICAL_YEARS) %>%
      group_by(region) %>%
      mutate(value = approx_fun(year, value, rule = 2), year = as.integer(year)) %>%
      ungroup() %>%
      mutate(type = "total") %>%
      bind_rows(L111.EIA_gas_offshore_MMcf %>% mutate(type = "offshore"),
                L111.EIA_gas_shale_MMcf %>% mutate(type = "shale"),
                L111.EIA_gas_coalbed_MMcf %>%  mutate(type = "coalbed")) %>%
      filter(region != "US (Agg)") -> L111.EIA_gas_MMcf

    # Make adjustments and back out conventional and tight gas production using US EIA production data.
    # Calculate conventional and tight gas production
    L111.EIA_gas_MMcf_wide <- L111.EIA_gas_MMcf %>%
      spread(type, value) %>%
      replace_na(list(total = 0, offshore = 0, shale = 0, coalbed = 0)) %>%
      mutate(conventional = 0) %>%
      mutate(remainder = total - offshore - shale - coalbed) %>%
      mutate(remainder = ifelse(grepl("OCS", region), 0, remainder)) -> L111.EIA_gas_MMcf

    L111.gas_supply_state_T_EJ %>% filter(resource == "tight gas", grade.1 > 0) %>%
      select(state, grade.1) -> L111.pot_tight_gas_regions

    # States that don't have any tight gas reserves probably weren't producing it
    # so label it as conventional.
    L111.EIA_gas_MMcf %>% left_join(L111.pot_tight_gas_regions, by = c("region" = "state")) %>%
      mutate(conventional = if_else(is.na(grade.1), conventional + remainder, conventional)) %>%
      mutate(remainder = if_else(is.na(grade.1), 0, remainder)) %>%
      select(-grade.1) -> L111.EIA_gas_MMcf

    # AEO doesn't have AK producing any shale nor tight so assume it is all conventional
    L111.EIA_gas_MMcf %>% mutate(conventional = if_else(region == "AK", conventional + remainder, conventional)) %>%
      mutate(remainder = if_else(region == "AK", 0, remainder)) -> L111.EIA_gas_MMcf


    # A bunch of steps and assumptions to assign tight gas
    # YO 2022: originally this was a function "assign_tight_gas", but given it was only used once and
    # it is fairly complicated, here just direct "unwrap" it into steps for better understanding
    # ------------------------------------------------------------------------------------------------------------
    state.d <- L111.EIA_gas_MMcf
    us.d <- EIA_gas_US_T_MMcf %>% gather_years()
    tg.supply <- L111.pot_tight_gas_regions

    # calculate shares of each type for each historical year in the US total
    us.d.hist <- us.d %>%
      complete(nesting(type), year = HISTORICAL_YEARS) %>%
      group_by(type) %>%
      mutate(value = approx_fun(year, value, rule = 2),year = as.integer(year)) %>%
      ungroup() %>%
      mutate(region = "USA") %>%
      group_by(region, year) %>%
      mutate(total = sum(value)) %>%
      mutate(share = value / total) %>%
      ungroup() %>%
      filter(year %in% state.d$year)

    # keep Alaska total
    ak.d <- state.d %>% filter(region == "AK") %>%
      mutate(type = "Alaska") %>%
      select(type, year, value = total)

    # sum of lower48 states (no AK)
    state.d.sum.noAK <- state.d %>%
      gather(type, value, -region, -year) %>%
      filter(type != "total") %>%
      filter(region != "AK") %>%
      mutate(region = "USA") %>%
      group_by(type, year) %>%
      summarise(value = sum(value)) %>%
      ungroup()

    state.d.sum.ak <- bind_rows(state.d.sum.noAK, ak.d)

    # compute share for each type
    share.comp <- state.d.sum.ak %>%
      group_by(year) %>%
      mutate(share.calc = value / sum(value)) %>%
      ungroup() %>%
      mutate(type = case_when(
        type == "offshore" ~ "Lower 48 offshore",
        type == "shale" ~ "Shale Gas",
        type == "coalbed" ~ "Coalbed methane",
        type == "conventional" ~ "Lower 48 onshore conventional",
        type == "remainder" ~ "Tight gas",
        TRUE ~ type
      )) %>%
      left_join_error_no_match(us.d.hist %>% select(type, year, share), by = c("type", "year")) %>%
      mutate(share.diff = share.calc - share)

    # relocate tight gas - determine the amount of Tight Gas to be relocated
    tg.to.move <- us.d.hist %>%
      select(type, year, total) %>%
      inner_join(share.comp %>% filter(type == "Tight gas"), by = c("type", "year")) %>%
      mutate(gas.diff = total * share.diff)

    # from state-level data filter out states with remainder > 0
    state.tg.available <- state.d %>%
      filter(remainder > 0) %>%
      select(region) %>%
      distinct()

    tg.supply <- L111.pot_tight_gas_regions %>%
      filter(state != "AK" & state %in% state.tg.available$region) %>%
      mutate(region = "USA") %>%
      group_by(region) %>%
      mutate(total = sum(grade.1)) %>%
      mutate(share.supply = grade.1 / total) %>%
      ungroup()

    tg.to.move.supply <- tg.to.move %>%
      repeat_add_columns(tg.supply %>% select(state, share.supply)) %>%
      mutate(tg.move = gas.diff * share.supply) %>%
      rename(region = state) %>%
      select(region, year, tg.move)

    state.d.relocated <- state.d %>%
      # use left_join because AK will be NA
      left_join(tg.to.move.supply, by = c("region", "year")) %>%
      replace_na(list(tg.move = 0)) %>%
      rename(tight = tg.move) %>%
      mutate(remainder = remainder - tight) %>%
      # TODO: what to do about remainder that went negative, we will be short by
      # this amount and we will have no conventional production.
      mutate(tight = ifelse(remainder < 0, tight + remainder, tight)) %>%
      mutate(remainder = ifelse(remainder < 0, 0, remainder)) %>%
      mutate(conventional = conventional + remainder) %>%
      select(-remainder)

    L111.EIA_gas_MMcf <- state.d.relocated

    # ------------------------------------------------------------------------------------------------------------
    # It seems state offshore resource are accounted under conventional in the USGS
    # resource supply so move them there.
    L111.EIA_gas_MMcf %>%
      mutate(conventional = if_else(!grepl('OCS', region), conventional + offshore, conventional)) %>%
      mutate(offshore = if_else(!grepl('OCS', region), 0, offshore)) -> L111.EIA_gas_MMcf


    # Additional hacks due to mismatch of calibration data and supply data
    # TODO this part needs to clean up
    L111.EIA_gas_MMcf %>% mutate(tight = if_else(region == "MT", tight + shale, tight)) %>%
      mutate(shale = if_else(region == "MT", 0, shale)) %>%
      mutate(conventional = if_else(region == "ND", conventional + shale, conventional)) %>%
      mutate(shale = if_else(region == "ND", 0, shale)) %>%
      mutate(tight = if_else(region == "AL", tight + coalbed, tight)) %>%
      mutate(coalbed = if_else(region == "AL", 0, coalbed)) %>%
      mutate(tight = if_else(region == "VA", tight + coalbed, tight)) %>%
      mutate(coalbed = if_else(region == "VA", 0, coalbed)) %>%
      mutate(conventional = if_else(region == "LA", conventional + coalbed, conventional)) %>%
      mutate(coalbed = if_else(region == "LA", 0, coalbed)) -> L111.EIA_gas_MMcf

    L111.EIA_gas_MMcf %>% select(-total) %>%
      gather(type, value, -region,- year) -> L111.EIA_gas_MMcf

    L111.EIA_gas_MMcf %>% mutate(value = value * CONV_MMCF_EJ) %>%
      rename(eia.prod = value) %>%
      filter(eia.prod > 0) %>%
      mutate(type = if_else(type != "coalbed", paste(type, "gas", sep = ' '), type)) %>%
      mutate(type = if_else(type == "coalbed", "coalbed methane", type)) -> L111.EIA_gas_EJ

    # Convert EIA data to shares
    # Convert EIA wellhead production data to shares
    L111.EIA_gas_EJ %>%
      left_join_error_no_match(L111.EIA_gas_EJ %>%
                                 group_by(year) %>%
                                 summarise(eia.total = sum(eia.prod)) %>%
                                 ungroup(), by = c("year")) %>%
      mutate(share = eia.prod / eia.total) -> L111.EIA_gas_EJ

    # Obtain GCAM USA data for natural gas production
    L111.Prod_EJ_R_F_Yh %>%
      filter(GCAM_region_ID == gcamusa.USA_REGION_NUMBER, fuel %in% c( "natural gas")) %>%
      group_by(year) %>%
      summarise(us.prod = sum(value)) %>%
      ungroup() -> L111.USA_gas_EJ

    # Use shares to downscale the GCAM USA natural gas production data to states
    L111.EIA_gas_EJ %>%
      left_join_error_no_match(L111.USA_gas_EJ, by = c("year")) %>%
      mutate(prod = us.prod * share) %>%
      rename(depresource = type) %>%
      mutate(subresource = depresource) -> L111.gas_prod_state_T_Yh_EJ

    # Format for output
    L111.gas_prod_state_T_Yh_EJ %>%
      select(state = region, depresource, subresource, year, prod) %>%
      mutate(state = if_else(state == "Gulf Coast OCS", "Gulf of Mexico OCS", state)) -> L111.gas_prod_state_T_Yh_EJ


    # ===================================================
    # Produce outputs

    L111.gas_supply_state_T_EJ %>%
      add_title("Natural gas supply by state and resource type") %>%
      add_units("EJ") %>%
      add_comments("Natural gas supply by state and resource type") %>%
      add_precursors("gcam-usa/USGS_gas_supply_Quad",
                     "gcam-usa/USGS_basin_state_mapping",
                     "gcam-usa/BOEM_gas_supply_EJ") ->
      L111.gas_supply_state_T_EJ

    L111.unconv_gas_supply_state_EJ %>%
      add_title("Remaining unconventional natural gas supply by state") %>%
      add_units("EJ") %>%
      add_comments("Remaining unconventional natural gas supply by state") %>%
      add_precursors("gcam-usa/USGS_gas_supply_Quad",
                     "gcam-usa/USGS_basin_state_mapping",
                     "gcam-usa/BOEM_gas_supply_EJ",
                     "L111.RsrcCurves_EJ_R_Ffos") ->
      L111.unconv_gas_supply_state_EJ

    L111.gas_prod_state_T_Yh_EJ %>%
      add_title("Downscaled to state US natural gas primary production by resource type / year") %>%
      add_units("EJ") %>%
      add_comments("Downscaled to state US natural gas primary production by resource type / year") %>%
      add_precursors("gcam-usa/EIA_gas_A_MMcf",
                     "gcam-usa/EIA_gas_offshore_MMcf",
                     "gcam-usa/EIA_gas_shale_MMcf",
                     "gcam-usa/EIA_gas_coalbed_MMcf",
                     "gcam-usa/EIA_gas_US_T_MMcf",
                     "L111.Prod_EJ_R_F_Yh") ->
      L111.gas_prod_state_T_Yh_EJ

    return_data(L111.gas_supply_state_T_EJ,
                L111.unconv_gas_supply_state_EJ,
                L111.gas_prod_state_T_Yh_EJ)
  } else {
    stop("Unknown command")
  }
}
