# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_LA111.rsrc_fos_Prod_USA
#'
#' Calculate historical fossil energy production and fossil resource supply curves for US states.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L111.ResCurves_EJ_R_Ffos_USA}, \code{L111.Prod_EJ_R_F_Yh_USA},
#'  The corresponding file in the original data system was \code{LA111.rsrc_fos_Prod.R} (energy level1).
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
             FILE = "gcam-usa/USGS_oil_supply_Quad",
             FILE = "gcam-usa/USGS_basin_state_mapping",
             FILE = "gcam-usa/BOEM_gas_supply_EJ",
             FILE = "gcam-usa/BOEM_oil_supply_EJ",
             FILE = "gcam-usa/EIA_coal_reserve_2021_Mton",
             FILE = "gcam-usa/ETSAP_gas_cost_range",
             FILE = "gcam-usa/ETSAP_oil_cost_range",
             FILE = "gcam-usa/ETSAP_coal_cost_range",
             FILE = "gcam-usa/BOEM_gas_cost",
             FILE = "gcam-usa/BOEM_oil_cost",
             FILE = "gcam-usa/EIA_gas_market_prod_state_MMcf_total",
             FILE = "gcam-usa/EIA_gas_market_prod_state_Bcf_coalbed",
             FILE = "gcam-usa/EIA_gas_market_prod_state_Bcf_shalegas",
             FILE = "gcam-usa/EIA_oil_market_prod_state_thousBBL_total",
             FILE = "gcam-usa/EIA_tight_oil_production_mbbl_per_day",
             FILE = "gcam-usa/EIA_tight_oil_play_state_mapping",
             FILE = "gcam-usa/EIA_oil_GOM_refineries_input_2022",
             FILE = "gcam-usa/Alaska_offshore_gas_oil_2021",
             FILE = "gcam-usa/EIA_coal_prod_state_ton_surface_2001_2021",
             FILE = "gcam-usa/EIA_coal_prod_state_ton_underground_2001_2021",
             FILE = "gcam-usa/EIA_NG_prod_mapping_total",
             FILE = "gcam-usa/EIA_NG_prod_mapping_coalbed",
             FILE = "gcam-usa/EIA_NG_prod_mapping_shalegas",
             FILE = "energy/A10.ResSubresourceProdLifetime",
             "L111.Prod_EJ_R_F_Yh",
             "L111.RsrcCurves_EJ_R_Ffos"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L111.ResCurves_EJ_R_Ffos_USA",
             "L111.Prod_EJ_R_F_Yh_USA"))
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
    USGS_oil_supply_Quad <- get_data(all_data, "gcam-usa/USGS_oil_supply_Quad")
    USGS_basin_state_mapping <- get_data(all_data, "gcam-usa/USGS_basin_state_mapping")
    BOEM_gas_supply_EJ <- get_data(all_data, "gcam-usa/BOEM_gas_supply_EJ")
    BOEM_oil_supply_EJ <- get_data(all_data, "gcam-usa/BOEM_oil_supply_EJ")
    EIA_coal_reserve_2021_Mton <- get_data(all_data, "gcam-usa/EIA_coal_reserve_2021_Mton")
    ETSAP_gas_cost_range <- get_data(all_data, "gcam-usa/ETSAP_gas_cost_range", strip_attributes = TRUE)
    ETSAP_oil_cost_range <- get_data(all_data, "gcam-usa/ETSAP_oil_cost_range", strip_attributes = TRUE)
    BOEM_gas_cost <- get_data(all_data, "gcam-usa/BOEM_gas_cost", strip_attributes = TRUE)
    BOEM_oil_cost <- get_data(all_data, "gcam-usa/BOEM_oil_cost", strip_attributes = TRUE)
    ETSAP_coal_cost_range <- get_data(all_data, "gcam-usa/ETSAP_coal_cost_range", strip_attributes = TRUE)
    EIA_gas_market_prod_state_MMcf_total <- get_data(all_data, "gcam-usa/EIA_gas_market_prod_state_MMcf_total")
    EIA_gas_market_prod_state_Bcf_coalbed <- get_data(all_data, "gcam-usa/EIA_gas_market_prod_state_Bcf_coalbed")
    EIA_gas_market_prod_state_Bcf_shalegas <- get_data(all_data, "gcam-usa/EIA_gas_market_prod_state_Bcf_shalegas")
    EIA_oil_market_prod_state_thousBBL_total <- get_data(all_data, "gcam-usa/EIA_oil_market_prod_state_thousBBL_total")
    EIA_tight_oil_production_mbbl_per_day <- get_data(all_data, "gcam-usa/EIA_tight_oil_production_mbbl_per_day")
    EIA_tight_oil_play_state_mapping <- get_data(all_data, "gcam-usa/EIA_tight_oil_play_state_mapping")
    EIA_oil_GOM_refineries_input_2022 <- get_data(all_data, "gcam-usa/EIA_oil_GOM_refineries_input_2022")
    Alaska_offshore_gas_oil_2021 <- get_data(all_data, "gcam-usa/Alaska_offshore_gas_oil_2021")
    EIA_coal_prod_state_ton_surface_2001_2021 <- get_data(all_data, "gcam-usa/EIA_coal_prod_state_ton_surface_2001_2021")
    EIA_coal_prod_state_ton_underground_2001_2021 <- get_data(all_data, "gcam-usa/EIA_coal_prod_state_ton_underground_2001_2021")
    EIA_NG_prod_mapping_total <- get_data(all_data, "gcam-usa/EIA_NG_prod_mapping_total")
    EIA_NG_prod_mapping_coalbed <- get_data(all_data, "gcam-usa/EIA_NG_prod_mapping_coalbed")
    EIA_NG_prod_mapping_shalegas <- get_data(all_data, "gcam-usa/EIA_NG_prod_mapping_shalegas")
    A10.ResSubresourceProdLifetime <- get_data(all_data, "energy/A10.ResSubresourceProdLifetime")
    L111.Prod_EJ_R_F_Yh <- get_data(all_data, "L111.Prod_EJ_R_F_Yh", strip_attributes = TRUE)
    L111.RsrcCurves_EJ_R_Ffos <- get_data(all_data, "L111.RsrcCurves_EJ_R_Ffos", strip_attributes = TRUE)

    # ===================================================
    # Perform computations

    # NOTE: FUNCTION STILL NEEDS TO BE RE-WRITTEN IN DPLYR
    compute_offshore_costs <- function( supply, costs ) {
      ret.costs <- data.frame()
      grades <- c( "grade.hist", "grade.1", "grade.2", "grade.3")
      supply$cumul.1 <- supply$grade.1
      supply$cumul.2 <- supply$cumul.1 + supply$grade.2
      supply$cumul.3 <- supply$cumul.2 + supply$grade.3
      for( r in unique( costs$region ) ) {
        costs.region <- subset( costs, region == r )
        supply.region <- subset( supply, region == r )
        lmfit <- lm( formula = price ~ sqrt( quantity ) + quantity, data=costs.region )
        ret.costs.region <- data.frame( region=r, grade=grades, cost=0 )
        min.cost <- min( costs.region$price ) + 1.5
        ret.costs.region$cost[1] <- min.cost * 0.5
        ret.costs.region$cost[2] <- min.cost * 0.9
        ret.costs.region$cost[3:4] <- predict( lmfit, data.frame( quantity=c( supply.region$cumul.2, supply.region$cumul.3 ) ) )
        # TODO: get better grasp of how "reserve adjustment factors" was taking into
        # account.  They claim a value of 0.4.
        ret.costs.region$cost[3] <- ret.costs.region$cost[3] * 0.6
        ret.costs.region$cost[3] <- pmax( ret.costs.region$cost[3], ret.costs.region$cost[2] * 1.1 )
        ret.costs.region$cost[4] <- ret.costs.region$cost[4] * 0.6
        ret.costs <- rbind( ret.costs, ret.costs.region )
      }
      return(ret.costs)
    }

    # Part 1a: historical production of natural gas
    # ------------------------------------------------------------------------------------------------------------------------------
    # clean up data
    # Federal offshore are aggregated into the nearest states
    # For single state: Federal offshore just combine with state offshore
    # for Federal Gulf of Mexico, these data are available between 1997-2021, just allocate them into the nearest three
    # Federal offshore states (data only available between 1992-1998): based on their average production share during  1991-1998

    # unconventional gas includes coalbed methane and shale gas, and their productions are separated reported by EIA
    # conventional gas is obtained by subtracting the unconventional gas from total onshore gas

    # 1.1a Split production into onshore and offshore

    # total production raw data
    L111.gas_production_raw <- EIA_gas_market_prod_state_MMcf_total %>%
      gather(category, value, -Date) %>%
      replace_na(list(value = 0)) %>%
      rename(year = Date) %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      left_join_error_no_match(EIA_NG_prod_mapping_total, by = "category") %>%
      filter(!state %in% c("OtherStates", "USA")) %>%
      mutate(value = value * CONV_MMCF_EJ)

    # define offshore states
    offshore_states <- L111.gas_production_raw %>%
      filter(type == "offshore") %>%
      select(state) %>%
      distinct() %>%
      # filter out Gulf of Mexico
      filter(state != "GOM")

    # clean up production data to split onshore and offshore
    L111.gas_production_onshore_offshore <- L111.gas_production_raw %>%
      # for states with offshore productions they already report onshore and offshore separately, so we don't need their "total"
      filter(!(state %in% offshore_states$state & type == "total")) %>%
      # for states without offshore productions, their "total" should be all "onshore"
      mutate(type = if_else(type == "total", "onshore", type))

    # 1.2a Split onshore into conventional and unconventional

    # coalbed methane
    L111.gas_production_coalbed <- EIA_gas_market_prod_state_Bcf_coalbed %>%
      gather(category, value, -Date) %>%
      replace_na(list(value = 0)) %>%
      rename(year = Date) %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      left_join_error_no_match(EIA_NG_prod_mapping_coalbed, by = "category") %>%
      filter(type == "total" & state != "USA") %>%
      # first convert billion cubic feet (Bcf) into million cubic feet (MMcf)
      mutate(value = value * CONV_BCF_MMCF  * CONV_MMCF_EJ) %>%
      select(state, year, value_coalbed = value)

    # shale gas
    L111.gas_production_shalegas <- EIA_gas_market_prod_state_Bcf_shalegas %>%
      gather(category, value, -Date) %>%
      replace_na(list(value = 0)) %>%
      rename(year = Date) %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      left_join_error_no_match(EIA_NG_prod_mapping_shalegas, by = "category") %>%
      filter(type == "total" & state != "USA") %>%
      # first convert billion cubic feet (Bcf) into million cubic feet (MMcf)
      mutate(value = value * CONV_BCF_MMCF  * CONV_MMCF_EJ) %>%
      select(state, year, value_shale = value)

    # split onshore gas into conventional and unconventional gas
    L111.gas_production_onshore <- L111.gas_production_onshore_offshore %>%
      filter(type == "onshore") %>%
      # use left_join because not all states have coalbed methane
      left_join(L111.gas_production_coalbed, by = c("year", "state")) %>%
      # use left_join because not all states have shale
      left_join(L111.gas_production_shalegas, by = c("year", "state")) %>%
      replace_na(list(value_coalbed = 0, value_shale = 0)) %>%
      mutate(`onshore unconventional` = value_coalbed + value_shale,
             `onshore conventional` = value - `onshore unconventional`) %>%
      select(state, year, `onshore unconventional`, `onshore conventional`) %>%
      gather(type, value, -state, -year) %>%
      # there are a few cases (such as ND) that have negative "onshore conventional", i.e.
      # the sum of unconventional production exceeds the total production
      # in this case we assume 0 conventional production
      mutate(value = if_else(value < 0, 0, value))

    # 1.3a clean up offshore production

    # process Golf of Mexico Federal into the neighboring three states that used to beyond "federal offshore"
    # state shares are based on average historical productions
    GOM_share <- L111.gas_production_onshore_offshore %>%
      filter(grepl("Federal Offshore", category) & state %in% c("AL", "LA", "TX")) %>%
      group_by(state) %>%
      summarise(value = mean(value)) %>%
      ungroup() %>%
      mutate(GOM_share = value / sum(value)) %>%
      select(state, GOM_share)

    # separate federal offshore GOM production by the neighboring three states (AL, LA, and TX)
    GOM_state_offshore <- L111.gas_production_onshore_offshore %>%
      filter(grepl("Federal Offshore", category) & state == "GOM") %>%
      repeat_add_columns(GOM_share) %>%
      mutate(value.GOM.state = value * GOM_share) %>%
      select(year, category, value = value.GOM.state, type, state = state.y)

    # combine with the rest "offshore" states
    L111.gas_production_offshore <- L111.gas_production_onshore_offshore %>%
      filter(type == "offshore" & state != "GOM") %>%
      bind_rows(GOM_state_offshore) %>%
      select(names(L111.gas_production_onshore)) %>%
      group_by(state, year, type) %>%
      summarise(value = sum(value)) %>%
      ungroup()

    # 1.4a combine onshore (conventional and unconventional) and offshore
    L111.gas_production_states_EJ_EIA <- L111.gas_production_onshore %>%
      bind_rows(L111.gas_production_offshore) %>%
      rename(region = state) %>%
      mutate(resource = "natural gas",
             reserve.subresource = paste0(type, " gas")) %>%
      group_by(region, resource, reserve.subresource, year) %>%
      summarise(eia.prod = sum(value)) %>%
      ungroup()

    # 1.5a use EIA produciton ratio to downscale national total values
    # Convert EIA wellhead production data to shares
    L111.gas_production_states_EJ_EIA %>%
      left_join_error_no_match(L111.gas_production_states_EJ_EIA %>%
                                 group_by(year) %>%
                                 summarise(eia.total = sum(eia.prod)) %>%
                                 ungroup(), by = c("year")) %>%
      mutate(share = eia.prod / eia.total) %>%
      replace_na(list(share = 0)) -> L111.EIA_gas_EJ

    # Obtain GCAM USA data for natural gas production
    L111.Prod_EJ_R_F_Yh %>%
      filter(GCAM_region_ID == gcamusa.USA_REGION_NUMBER, fuel %in% c( "natural gas")) %>%
      group_by(year) %>%
      summarise(us.prod = sum(value)) %>%
      ungroup() -> L111.USA_gas_EJ

    # Use shares to downscale the GCAM USA natural gas production data to states
    L111.EIA_gas_EJ %>%
      left_join_error_no_match(L111.USA_gas_EJ, by = c("year")) %>%
      mutate(value = us.prod * share) %>%
      select(region, resource, reserve.subresource, year, value) -> L111.gas_production_states_EJ


    # Part 1b: historical production of crude oil
    # ------------------------------------------------------------------------------------------------------------------------------
    # clean up data
    # Federal offshore are aggregated into the nearest states
    # For PADD5: Federal offshore productions are split into California and Alaska, and California bans future offshore oil production
    # for Federal Gulf of Mexico (PADD3), we allocate them into the nearest three states

    # there is no unconventional oil in the US, so crude oil production is split into onshore and offshore

    # 1.1b Split production into onshore and offshore

    # total production raw data
    L111.oil_production_raw_EJ <- EIA_oil_market_prod_state_thousBBL_total %>%
      gather(category, value, -Year) %>%
      replace_na(list(value = 0)) %>%
      rename(year = Year) %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      # rename PADD5 federal offshore into AK_CA for clean up
      mutate(category = gsub("Federal Offshore PADD 5", "Federal Offshore--Alaska and California", category)) %>%
      # filter out aggregated Petroleum Administration for Defense Districts (PADD) regions or national total
      filter(!grepl("PADD|U.S.", category)) %>%
      # filter Alaska total to avoid double counting
      # Alaska reports both total and south/north, just need to keep one of them
      filter(!grepl("Alaska Field Production of Crude Oil thousBBL", category)) %>%
      # clean up columns
      mutate(category = gsub(" Field Production of Crude Oil thousBBL", "", category)) %>%
      mutate(type = if_else(grepl("Offshore", category), "offshore", "onshore")) %>%
      # use left_join because offshore regions are still aggregated
      left_join(tibble(category = state.name, state = state.abb), by = "category") %>%
      mutate(state = case_when(
        grepl("Gulf of Mexico", category) ~ "GOM",
        # according to Alaska Oil and Gas Conservation Commission Offshore Wells list, all AK "ARCTIC OCEAN, FED."
        # wells are categorized as "Exploratory", which means AK doesn't have offshore oil production from federal water
        # thus here assign all federal offshore oil of PADD5 into CA offshore
        grepl("Alaska and California", category) ~ "CA",
        # combine Alaska field north and south into AK
        grepl("Alaska", category) ~ "AK",
        T ~ state
      )) %>%
      group_by(state, type, year) %>%
      summarise(value = sum(value)) %>%
      ungroup %>%
      # convert thousand bbl into million bbl then convert into EJ
      mutate(value = value * 1e-3 * CONV_MBBL_EJ)

    # 1.2b Split AK offshore oil production from state water, based on Alaska Oil and Gas Conservation Commission Offshore data
    # here some AK production from North Slope come from state offshore water (within 3 miles of the coast)
    # manually split them based on AK's own data
    L111.oil_production_raw_EJ_AK_split <- L111.oil_production_raw_EJ %>%
      filter(state == "AK") %>%
      left_join(Alaska_offshore_gas_oil_2021 %>% filter(ProductionType == "Oil Production") %>%
                  select(year, oilprod_EJ) %>%
                  rename(offshore = oilprod_EJ), by = "year") %>%
      replace_na(list("offshore" = 0)) %>%
      mutate(onshore = value - offshore) %>%
      select(state, year, onshore, offshore) %>%
      gather(type, value, -state, -year)

    # combine with original onshore/offshore data
    L111.oil_production_clean_EJ <- L111.oil_production_raw_EJ %>%
      filter(state != "AK") %>%
      bind_rows(L111.oil_production_raw_EJ_AK_split)

    # 1.3b Split onshore into conventional and unconventional (tight oil)
    EIA_tight_oil_produciton_EJ <- EIA_tight_oil_production_mbbl_per_day %>%
      gather(oil_play, mbbl_per_day, -Date) %>%
      separate(Date, into = c("days", "month", "year"), sep = "-") %>%
      # since the date here indicate the last day of each month, it also indicates the number of days in each month
      mutate(days = as.integer(days)) %>%
      # assume the same daily production over the entire month as the last day's daily production
      mutate(mbbl_per_month = mbbl_per_day * days) %>%
      # paste year into full year
      # here the earlier year available is 2000, so just paste "20" to each year
      mutate(year = as.integer(paste0("20", year))) %>%
      # obtain yearly production for each play
      group_by(oil_play, year) %>%
      summarise(mbbl_per_year = sum(mbbl_per_month)) %>%
      ungroup() %>%
      # use left_join because same play will be mapped to multiple states
      left_join(EIA_tight_oil_play_state_mapping, by = "oil_play") %>%
      mutate(value_EJ = mbbl_per_year * share * CONV_MBBL_EJ) %>%
      group_by(state, year) %>%
      summarise(value_tight_oil_EJ = sum(value_EJ)) %>%
      ungroup()

    L111.oil_production_onshore <- L111.oil_production_clean_EJ %>%
      filter(type == "onshore") %>%
      # use left_join because not all states have unconventional oil so NA will exist
      left_join(EIA_tight_oil_produciton_EJ, by = c("state", "year")) %>%
      replace_na(list(value_tight_oil_EJ = 0)) %>%
      # the EIA_tight_oil_play_state_mapping has been adjusted to make sure tight oil (unconventional oil) is less than total production
      mutate(value_conv_oil = value - value_tight_oil_EJ) %>%
      select(state, year, value_conv_oil, value_tight_oil_EJ) %>%
      gather(var, value, -state, -year) %>%
      mutate(type = if_else(var == "value_conv_oil", "onshore conventional", "onshore unconventional")) %>%
      select(names(L111.oil_production_clean_EJ))

    # 1.4b Split offshore into states

    # define offshore states
    # GOM region, split federal GOM production into neighbor states based on coastal refinery input (EIA)
    # shares are calculated as year-specific starting from 1985
    offshore_states_oil_GOM_shares <- EIA_oil_GOM_refineries_input_2022 %>%
      gather(RefiningDistrict, value, -Year) %>%
      # keep gulf coastal refining district only
      # filter out PADD 3 total
      filter(grepl("Gulf Coast", RefiningDistrict) & !grepl("PADD 3", RefiningDistrict)) %>%
      # assume all GOM oil (offshore oil) are produced by TX and LA
      # shares based on their refinery input
      mutate(state = if_else(grepl("Texas", RefiningDistrict), "TX", "LA")) %>%
      select(-RefiningDistrict) %>%
      group_by(Year) %>%
      mutate(share = value / sum(value)) %>%
      ungroup() %>%
      mutate(region = "GOM", type = "offshore") %>%
      rename(year = Year) %>%
      select(-value) %>%
      # extrapolate historical shares to all HISTORICAL_YEARS (starting from 1971)
      complete(nesting(region, type, state), year = HISTORICAL_YEARS) %>%
      group_by(region, type, state) %>%
      # using the earliest available year
      mutate(share = approx_fun(year, share, rule = 2)) %>%
      ungroup()

    L111.oil_production_offshore <- L111.oil_production_clean_EJ %>%
      filter(type == "offshore") %>%
      rename(region = state, value_agg = value) %>%
      # use left_join because we need to copy offshore regions into states (# of rows changed)
      # also here just apply GOM shares, AK and CA already split out
      left_join(offshore_states_oil_GOM_shares, by = c("region", "type", "year")) %>%
      # fill in AK and CA, and their shares as 1s
      mutate(state = if_else(is.na(state), region, state),
             share = if_else(is.na(share), 1.0, share)) %>%
      # apply year-specific shares to aggregated production for offshore regions
      mutate(value = value_agg * share) %>%
      select(names(L111.oil_production_clean_EJ))

    # 1.5b combine onshore (conventional and unconventional) and offshore
    L111.oil_production_states_EJ_EIA <- bind_rows(L111.oil_production_onshore,
                                                   L111.oil_production_offshore) %>%
      rename(region = state) %>%
      mutate(resource = "crude oil",
             reserve.subresource = paste0(type, " oil")) %>%
      group_by(region, resource, reserve.subresource, year) %>%
      summarise(eia.prod = sum(value)) %>%
      ungroup()

    # 1.6b use EIA production ratio to downscale national total values
    # Convert EIA wellhead production data to shares
    L111.oil_production_states_EJ_EIA %>%
      left_join_error_no_match(L111.oil_production_states_EJ_EIA %>%
                                 group_by(year) %>%
                                 summarise(eia.total = sum(eia.prod)) %>%
                                 ungroup(), by = c("year")) %>%
      mutate(share = eia.prod / eia.total) %>%
      replace_na(list(share = 0)) -> L111.EIA_oil_EJ

    # Obtain GCAM USA data for crude oil production
    L111.Prod_EJ_R_F_Yh %>%
      filter(GCAM_region_ID == gcamusa.USA_REGION_NUMBER, fuel %in% c( "crude oil")) %>%
      group_by(year) %>%
      summarise(us.prod = sum(value)) %>%
      ungroup() -> L111.USA_oil_EJ

    # Use shares to downscale the GCAM USA crude oil production data to states
    L111.EIA_oil_EJ %>%
      left_join_error_no_match(L111.USA_oil_EJ, by = c("year")) %>%
      mutate(value = us.prod * share) %>%
      select(region, resource, reserve.subresource, year, value) -> L111.oil_production_states_EJ


    # Part 1c: historical production of coal
    # ------------------------------------------------------------------------------------------------------------------------------
    # clean up data
    # EIA coal data browser include historical coal production since 2001
    # TODO: earlier historical production by state from 1983 are available from individual raw tables (each table contains one year)
    # need to process them one by one
    # coal production are separated by surface and underground mining

    # 1.1c clean up raw data into EIA totals by state and mining type
    L111.coal_production_states_EJ_EIA <- bind_rows(EIA_coal_prod_state_ton_surface_2001_2021 %>%
                                                      mutate(type = "surface mining"),
                                                    EIA_coal_prod_state_ton_underground_2001_2021 %>%
                                                      mutate(type = "underground mining")) %>%
      gather(year, value, -state, -type) %>%
      mutate(year = as.integer(year)) %>%
      replace_na(list(value = 0)) %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      # clean-up state names (remove US-)
      mutate(region = gsub("US-", "", state)) %>%
      # convert short ton coal (of equivalent) to EJ
      mutate(eia.prod = value * CONV_TCE_EJ) %>%
      mutate(resource = "coal",
             reserve.subresource = type) %>%
      select(region, resource, reserve.subresource, year, eia.prod)

    # 1.2c use EIA production ratio to downscale national total values
    # Convert EIA wellhead production data to shares
    L111.coal_production_states_EJ_EIA %>%
      left_join_error_no_match(L111.coal_production_states_EJ_EIA %>%
                                 group_by(year) %>%
                                 summarise(eia.total = sum(eia.prod)) %>%
                                 ungroup(), by = c("year")) %>%
      mutate(share = eia.prod / eia.total) %>%
      select(-eia.prod, -eia.total) %>%
      replace_na(list(share = 0)) -> L111.EIA_coal_EJ_incomplete_year

    # extrapolate to all historical years
    # for GCAM historical years earlier than EIA's earliest year, use the earliest available shares
    L111.EIA_coal_EJ <- L111.EIA_coal_EJ_incomplete_year %>%
      # extrapolate historical shares to all HISTORICAL_YEARS (starting from 1971)
      complete(nesting(region, resource, reserve.subresource), year = HISTORICAL_YEARS) %>%
      group_by(region, resource, reserve.subresource) %>%
      # using the earliest available year
      mutate(share = approx_fun(year, share, rule = 2)) %>%
      ungroup()

    # Obtain GCAM USA data for coal production
    L111.Prod_EJ_R_F_Yh %>%
      filter(GCAM_region_ID == gcamusa.USA_REGION_NUMBER, fuel %in% c( "coal")) %>%
      group_by(year) %>%
      summarise(us.prod = sum(value)) %>%
      ungroup() -> L111.USA_coal_EJ

    # Use shares to downscale the GCAM USA coal production data to states
    L111.EIA_coal_EJ %>%
      left_join_error_no_match(L111.USA_coal_EJ, by = c("year")) %>%
      mutate(value = us.prod * share) %>%
      select(region, resource, reserve.subresource, year, value) -> L111.coal_production_states_EJ

    # Part 2a: gas supply (supply by grade)
    # ------------------------------------------------------------------------------------------------------------------------------

    # Map supply from basins to states.
    L111.gas_prob_names <- names( USGS_gas_supply_Quad )[ grep('^F\\d\\d$', names( USGS_gas_supply_Quad ), perl=TRUE ) ]

    USGS_basin_state_mapping %>%
      # number of rows will change since one basin could be mapped to multiple states
      left_join(USGS_gas_supply_Quad, by = c("basin")) %>%
      gather(type, value, -state, -basin, -fraction, -row, -resource) %>%
      # 1 quad = 10^15 Btu = 1.055 EJ
      mutate(value = value * fraction * CONV_BTU_KJ ) %>%
      # aggregate all types into the onshore natural gas
      rename(region = state) %>%
      mutate(reserve.subresource = case_when(
        grepl("conventional", resource) ~ "onshore conventional gas",
        T ~ "onshore unconventional gas"
      )) %>%
      mutate(resource = "natural gas") %>%
      group_by(region, resource, reserve.subresource, type) %>%
      summarise(value = sum(value)) %>%
      ungroup() -> L111.gas_supply_state_T_EJ_onshore

    # The supply is given as total cumulative production at a given probability,
    # here we need the grades to be the "additional supply" available in that grade.
    L111.gas_supply_state_T_EJ_onshore %>%
      spread(type, value) %>%
      mutate(grade.1 = F95, grade.2 = F50 - F95, grade.3 = F05 - F50) %>%
      select(-one_of(L111.gas_prob_names)) -> L111.gas_supply_state_T_EJ_onshore_wide

    # process offshore gas supply from BOEM
    L111.gas_supply_state_T_EJ_offshore_wide <- BOEM_gas_supply_EJ %>%
      # According to BOEM, there is no active offshore lease in Atlantic area since 1984
      # so we don't assume these neighboring states will have offshore gas supply
      # Source: https://www.boem.gov/oil-gas-energy/oil-and-gas-atlantic (last accessed Jan 2023)
      filter(region != "Atlantic OCS") %>%
      mutate(region = case_when(
        region == "Alaska OCS" ~ "AK",
        region == "Pacific OCS" ~ "CA",
        region == "Gulf of Mexico OCS" ~ "GOM"
      )) %>%
      gather(grade, value, -region) %>%
      # still use the historical production share of GOM states as the proxy for supply
      # use left_join because AK and CA will be NAs
      left_join(GOM_share %>% mutate(region = "GOM"), by = "region") %>%
      mutate(state = if_else(is.na(state), region, state),
             GOM_share = if_else(is.na(GOM_share), 1.0, GOM_share)) %>%
      mutate(value = value * GOM_share) %>%
      mutate(resource = "natural gas",
             reserve.subresource = "offshore gas") %>%
      select(region = state, resource, reserve.subresource, grade, value) %>%
      spread(grade, value)

    # combine with onshore and offshore gas supply
    L111.gas_supply_state_T_EJ_onshore_wide %>%
      bind_rows(L111.gas_supply_state_T_EJ_offshore_wide) -> L111.gas_supply_state_T_EJ_wide

    # harmonize with the national supply
    # 1a) create state shares from USGS data
    L111.gas_supply_state_T_EJ_wide %>%
      mutate(total = grade.1 + grade.2 + grade.3) %>%
      group_by(region) %>%
      summarise(total = sum(total)) %>%
      mutate(region_sum = sum(total)) %>%
      mutate(share = total / region_sum) %>%
      select(-total, -region_sum) -> L111.additional_gas_supply_downscale

    # 2a) sum all natural gas resources to national total (USGS and BOEM data).
    L111.gas_supply_state_T_EJ_wide %>%
      mutate(GCAM_region_ID = gcamusa.USA_REGION_NUMBER) %>%
      group_by(GCAM_region_ID) %>%
      summarise(USGS_total = sum(grade.1, grade.2, grade.3)) %>%
      ungroup() -> L111.gas_supply_USA_USGS_BOEM_T_EJ

    # 3a) Subtract from energy data system natural gas resource curve (L111.RsrcCurves_EJ_R_Ffos)
    # to get remaining natural gas resources.
    L111.RsrcCurves_EJ_R_Ffos %>%
      filter(GCAM_region_ID == gcamusa.USA_REGION_NUMBER) %>%
      filter(resource == "natural gas") %>%
      mutate(grade = as.numeric(gsub("grade", "", grade))) %>%
      mutate(cum_avail = cumsum(available)) %>%
      left_join_error_no_match(L111.gas_supply_USA_USGS_BOEM_T_EJ, by = c("GCAM_region_ID")) %>%
      mutate(cum_avail_remain = cum_avail - USGS_total) %>%
      filter(cum_avail_remain >= 0) %>%
      # reset the starting point of the lowest grade as the cumulative remaining of "additional gas"
      mutate(available = if_else(grade == min(grade), cum_avail_remain, available)) %>%
      select(-cum_avail, -USGS_total, -cum_avail_remain) %>%
      mutate(grade = paste("grade", grade, sep = ' ')) ->
      L111.RsrcCurves_EJ_R_gas_additional

    # Apply state shares to remaining natural gas resources
    additional_gas_supply_states <- distinct(L111.additional_gas_supply_downscale %>% select(region))

    L111.RsrcCurves_EJ_R_gas_additional %>%
      # drop the highest zero-available for now, which will be add later together
      filter(available > 0) %>%
      repeat_add_columns(additional_gas_supply_states) %>%
      left_join_error_no_match(L111.additional_gas_supply_downscale, by = c("region")) %>%
      mutate(available = available * share) %>%
      mutate(resource = "natural gas",
             # assuming additional supply are conventional gas
             reserve.subresource = "onshore conventional gas") %>%
      select(region, resource, reserve.subresource, grade, extractioncost, available) -> L111.additional_gas_supply_state_EJ



    # Part 2b: oil supply (supply by grade)
    # ------------------------------------------------------------------------------------------------------------------------------

    # Map supply from basins to states.
    USGS_basin_state_mapping %>%
      # number of rows will change since one basin could be mapped to multiple states
      left_join(USGS_oil_supply_Quad, by = c("basin")) %>%
      gather(type, value, -state, -basin, -fraction, -row, -resource) %>%
      # 1 quad = 10^15 Btu = 1.055 EJ
      mutate(value = value * fraction * CONV_BTU_KJ ) %>%
      # aggregate all types into the onshore oil
      rename(region = state) %>%
      mutate(reserve.subresource = case_when(
        grepl("unconventional", resource) ~ "onshore unconventional oil",
        T ~ "onshore conventional oil"
      )) %>%
      mutate(resource = "crude oil") %>%
      group_by(region, resource, reserve.subresource, type) %>%
      summarise(value = sum(value)) %>%
      ungroup() -> L111.oil_supply_state_T_EJ_onshore

    # The supply is given as total cumulative production at a given probability,
    # here we need the grades to be the "additional supply" available in that grade.
    L111.oil_supply_state_T_EJ_onshore %>%
      spread(type, value) %>%
      mutate(grade.1 = F95, grade.2 = F50 - F95, grade.3 = F05 - F50) %>%
      # here the names ("F95", "F50", "F05") are the same as L111.gas_prob_names
      select(-one_of(L111.gas_prob_names)) -> L111.oil_supply_state_T_EJ_onshore_wide

    # process offshore oil supply from BOEM
    L111.oil_supply_state_T_EJ_offshore_wide <- BOEM_oil_supply_EJ %>%
      # According to BOEM, there is no active offshore lease in Atlantic area since 1984
      # so we don't assume these neighboring states will have offshore oil supply
      # Source: https://www.boem.gov/oil-gas-energy/oil-and-gas-atlantic (last accessed Jan 2023)
      filter(region != "Atlantic OCS") %>%
      mutate(region = case_when(
        region == "Alaska OCS" ~ "AK",
        region == "Pacific OCS" ~ "CA",
        region == "Gulf of Mexico OCS" ~ "GOM"
      )) %>%
      gather(grade, value, -region) %>%
      # use the historical production share of GOM states as the proxy for supply
      # use left_join since here just map GOM states
      left_join(offshore_states_oil_GOM_shares %>%
                  group_by(region, state) %>%
                  summarise(share = mean(share)) %>%
                  ungroup, by = "region") %>%
      mutate(state = if_else(is.na(state), region, state),
             share = if_else(is.na(share), 1.0, share)) %>%
      mutate(value = value * share) %>%
      mutate(resource = "crude oil",
             reserve.subresource = "offshore oil") %>%
      select(region = state, resource, reserve.subresource, grade, value) %>%
      spread(grade, value)

    # combine with onshore and offshore oil supply
    L111.oil_supply_state_T_EJ_onshore_wide %>%
      bind_rows(L111.oil_supply_state_T_EJ_offshore_wide) -> L111.oil_supply_state_T_EJ_wide

    # harmonize with the national supply
    # 1b) create state shares from USGS data
    L111.oil_supply_state_T_EJ_wide %>%
      mutate(total = grade.1 + grade.2 + grade.3) %>%
      group_by(region) %>%
      summarise(total = sum(total)) %>%
      mutate(region_sum = sum(total)) %>%
      mutate(share = total / region_sum) %>%
      select(-total, -region_sum) -> L111.additional_oil_supply_downscale

    # 2b) sum all oil resources to national total (USGS and BOEM data).
    L111.oil_supply_state_T_EJ_wide %>%
      mutate(GCAM_region_ID = gcamusa.USA_REGION_NUMBER) %>%
      mutate(subresource = ifelse(grepl("onshore conventional oil", reserve.subresource), "crude oil", "unconventional oil")) %>%
      group_by(GCAM_region_ID, subresource) %>%
      summarise(USGS_total = sum(grade.1, grade.2, grade.3)) %>%
      ungroup() -> L111.oil_supply_USA_USGS_BOEM_T_EJ

    # 3b) Subtract from energy data system oil resource curve (L111.RsrcCurves_EJ_R_Ffos)
    # to get remaining oil resources.
    L111.RsrcCurves_EJ_R_Ffos %>%
      filter(GCAM_region_ID == gcamusa.USA_REGION_NUMBER) %>%
      filter(resource == "crude oil") %>%
      mutate(grade = as.numeric(gsub("grade", "", grade))) %>%
      mutate(cum_avail = cumsum(available)) %>%
      left_join_error_no_match(L111.oil_supply_USA_USGS_BOEM_T_EJ, by = c("GCAM_region_ID", "subresource")) %>%
      mutate(cum_avail_remain = cum_avail - USGS_total) %>%
      filter(cum_avail_remain >= 0) %>%
      # reset the starting point of the lowest grade as the cumulaitve remaining of "additional oil"
      mutate(available = if_else(grade == min(grade), cum_avail_remain, available)) %>%
      select(-cum_avail, -USGS_total, -cum_avail_remain) %>%
      mutate(grade = paste("grade", grade, sep = ' ')) ->
      L111.RsrcCurves_EJ_R_oil_additional

    # Apply state shares to remaining oil resources
    additional_oil_supply_states <- distinct(L111.additional_oil_supply_downscale %>% select(region))

    L111.RsrcCurves_EJ_R_oil_additional %>%
      # drop the highest zero-available for now, which will be add later together
      filter(available > 0) %>%
      repeat_add_columns(additional_oil_supply_states) %>%
      left_join_error_no_match(L111.additional_oil_supply_downscale, by = c("region")) %>%
      mutate(available = available * share) %>%
      mutate(resource = "crude oil",
             # assuming additional supply are onshore oil
             reserve.subresource = case_when(
               subresource == "crude oil" ~ "onshore conventional oil",
               subresource == "unconventional oil" ~ "onshore unconventional oil")) %>%
      select(region, resource, reserve.subresource, grade, extractioncost, available) -> L111.additional_oil_supply_state_EJ


    # Part 2c: coal supply (supply by grade)
    # ------------------------------------------------------------------------------------------------------------------------------

    # clean up coal supply by state
    EIA_coal_reserve_2021_Mton %>%
      gather(type, value, -region) %>%
      left_join(tibble(region = state.name, state = state.abb), by = "region") %>%
      filter(!grepl("total", type)) %>%
      # create reserve.subresource
      mutate(reserve.subresource = case_when(
        grepl("surface", type) ~ "surface mining",
        grepl("underground", type) ~ "underground mining",
      )) %>%
      # clean up resource grades
      mutate(type = gsub("surface.|underground.", "", type)) %>%
      mutate(resource = "coal") %>%
      # convert short ton coal (of equivalent) to EJ
      mutate(value = value * CONV_TCE_EJ * 1e6) %>%
      select(region = state, resource, reserve.subresource, type, value) -> L111.coal_supply_state_T_EJ_raw

    # special treatment of Arizona: non-zero historical production but zero supply
    # ----------------
    # fill in zero supply for all these higher grades, and then just add grade.hist later to cover its historical supply
    # Special case of AZ: Arizona has no current coal production.
    # However, areas in the northeastern part of the state on the Navajo and Hopi reservations and in east-central Arizona
    # have some coal.104 The state's last coal mine, the Kayenta mine, ceased operations in 2019 because its only customer,
    # the coal-fired Navajo Generating Station, closed.
    # source: https://www.eia.gov/state/print.php?sid=AZ
    NoFutureCoal_states <- L111.coal_production_states_EJ %>%
      anti_join(L111.coal_supply_state_T_EJ_raw, by = c("region", "resource", "reserve.subresource")) %>%
      group_by(region, resource, reserve.subresource) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(value > 0) %>%
      select(-value)

    L111.coal_supply_state_T_EJ_nofuture <- NoFutureCoal_states %>%
      # use left_join as number of rows will change
      # need to map all three types: recoverable, estimated.recoverable, and estimated.recoverable
      left_join(L111.coal_supply_state_T_EJ_raw %>% select(resource, reserve.subresource, type) %>% distinct(),
                by = c("resource", "reserve.subresource")) %>%
      mutate(value = 0)

    # combine with the original supply table
    L111.coal_supply_state_T_EJ <- bind_rows(L111.coal_supply_state_T_EJ_raw,
                                             L111.coal_supply_state_T_EJ_nofuture)

    # here we assume the following direct mapping from EIA definition to GCAM grades
    # Recoverable Coal Reserves at Producing Mines - grade 1
    # Estimated Recoverable Reserves - grade 2
    # Demonstrated Reserve Base - grade
    L111.coal_supply_state_T_EJ %>%
      spread(type, value) %>%
      rename(grade.1 = recoverable, grade.2 = estimated.recoverable, grade.3 = demonstrated) ->
      L111.coal_supply_state_T_EJ_wide

    # harmonize with the national supply
    # 1c) create state shares from EIA data
    L111.coal_supply_state_T_EJ_wide %>%
      mutate(total = grade.1 + grade.2 + grade.3) %>%
      group_by(region) %>%
      summarise(total = sum(total)) %>%
      mutate(region_sum = sum(total)) %>%
      mutate(share = total / region_sum) %>%
      select(-total, -region_sum) -> L111.additional_coal_supply_downscale

    # 2c) sum all coal resources to national total (EIA data).
    L111.coal_supply_state_T_EJ_wide %>%
      mutate(GCAM_region_ID = gcamusa.USA_REGION_NUMBER) %>%
      mutate(subresource = resource) %>%
      group_by(GCAM_region_ID, subresource) %>%
      summarise(EIA_total = sum(grade.1, grade.2, grade.3)) %>%
      ungroup() -> L111.coal_supply_USA_EIA_T_EJ

    # 3c) Subtract from energy data system coal resource curve (L111.RsrcCurves_EJ_R_Ffos)
    # to get remaining coal resources.
    L111.RsrcCurves_EJ_R_Ffos %>%
      filter(GCAM_region_ID == gcamusa.USA_REGION_NUMBER) %>%
      filter(resource == "coal") %>%
      mutate(grade = as.numeric(gsub("grade", "", grade))) %>%
      mutate(cum_avail = cumsum(available)) %>%
      left_join_error_no_match(L111.coal_supply_USA_EIA_T_EJ, by = c("GCAM_region_ID", "subresource")) %>%
      mutate(cum_avail_remain = cum_avail - EIA_total) %>%
      filter(cum_avail_remain >= 0) %>%
      # reset the starting point of the lowest grade as the cumulaitve remaining of "additional oil"
      mutate(available = if_else(grade == min(grade), cum_avail_remain, available)) %>%
      select(-cum_avail, -EIA_total, -cum_avail_remain) %>%
      mutate(grade = paste("grade", grade, sep = ' ')) ->
      L111.RsrcCurves_EJ_R_coal_additional

    # Apply state shares to remaining coal resources
    additional_coal_supply_states <- distinct(L111.additional_coal_supply_downscale %>% select(region))

    L111.RsrcCurves_EJ_R_coal_additional %>%
      # drop the highest zero-available for now, which will be add later together
      filter(available > 0) %>%
      repeat_add_columns(additional_coal_supply_states) %>%
      left_join_error_no_match(L111.additional_coal_supply_downscale, by = c("region")) %>%
      mutate(available = available * share) %>%
      mutate(resource = "coal",
             # assuming additional supply are underground coal (as surface coal are easier to extract)
             reserve.subresource = "underground mining") %>%
      select(region, resource, reserve.subresource, grade, extractioncost, available) -> L111.additional_coal_supply_state_EJ

    # Part 3a: gas supply cost (extraction cost by grade)
    # ------------------------------------------------------------------------------------------------------------------------------

    # define a function to clean up grade names: grade.hist, grade.1, grade.2...
    clean_grade <- function(df){
      df_grade <- df %>%
        group_by(region, reserve.subresource) %>%
        mutate(grade.order = row_number() -1 ) %>%
        ungroup() %>%
        mutate(grade = if_else(grade.order == 0, "grade.hist", paste0("grade.", grade.order))) %>%
        select(-grade.order)
      return(df_grade)
    }

    # resource curve
    # First need to specify resource costs
    # TODO: cost simple approximation methods are largely consistent with RIAM, needs a little more description

    # 1a) Start with onshore cost estimates
    ETSAP_gas_cost_range %>%
      mutate(reserve.subresource = case_when(
        grepl("conventional", type) ~ "onshore conventional gas",
        T ~ "onshore unconventional gas"
      )) %>%
      # take the average of the different unconventional gas types as the cost for unconventional natural gas
      group_by(reserve.subresource) %>%
      summarise(low_cost = mean(low_cost),
                high_cost = mean(high_cost)) %>%
      ungroup() %>%
      mutate(grade.hist = low_cost,
             grade.1 = low_cost + 1 * (high_cost - low_cost) / 3,
             grade.2 = low_cost + 2 * (high_cost - low_cost) / 3,
             grade.3 = high_cost) %>%
      gather(grade, cost, -reserve.subresource) %>%
      filter(grepl("grade", grade)) %>%
      mutate(cost = cost * gdp_deflator(1975, 2008)) -> L111.GradeCost.onshore

    # Duplicate costs by state and put on and off shore together
    L111.gas_supply_state_T_EJ_wide %>%
      filter(grepl( "onshore", reserve.subresource )) %>%
      distinct(region) -> onshore_states

    L111.onshore_regions <- unique(onshore_states$region)

    L111.GradeCost.onshore %>%
      repeat_add_columns(tibble("region" = L111.onshore_regions)) %>%
      select(region, reserve.subresource, grade, cost) -> L111.GradeCost.onshore

    # adjust additional onshore cost
    L111.additional_gas_supply_state_EJ %>%
      rename(cost = extractioncost) %>%
      select(region, reserve.subresource, grade, cost) -> L111.GradeCost.onshore.additional

    # shift the cost curve of the additional part higher than the highest cost of ETSAP range
    cost.grade.highest.gas <- L111.GradeCost.onshore.additional %>%
      group_by(reserve.subresource) %>%
      filter(cost == max(cost)) %>%
      ungroup() %>%
      select(reserve.subresource, cost.grade.highest = cost) %>%
      distinct()

    L111.GradeCost.onshore.additional_adjusted <- L111.GradeCost.onshore.additional %>%
      left_join_error_no_match(cost.grade.highest.gas, by = c("reserve.subresource")) %>%
      mutate(cost = cost + cost.grade.highest) %>%
      select(names(L111.GradeCost.onshore.additional))

    # 2a) Offshore cost estimates are more detailed however we will just simply these
    # separate OCS quantity by state shares, copying the same price

    BOEM_gas_cost %>%
      # According to BOEM, there is no active offshore lease in Atlantic area since 1984
      # so we don't assume these neighboring states will have offshore gas supply
      # Source: https://www.boem.gov/oil-gas-energy/oil-and-gas-atlantic (last accessed Jan 2023)
      filter(region != "Atlantic OCS") %>%
      mutate(region = case_when(
        region == "Alaska OCS" ~ "AK",
        region == "Pacific OCS" ~ "CA",
        region == "Gulf of Mexico OCS" ~ "GOM"
      )) %>%
      # still use the historical production share of GOM states as the proxy for supply
      # use left_join because AK and CA will be NAs
      left_join(GOM_share %>% mutate(region = "GOM"), by = "region") %>%
      mutate(state = if_else(is.na(state), region, state),
             GOM_share = if_else(is.na(GOM_share), 1.0, GOM_share)) %>%
      mutate(quantity = quantity * GOM_share) %>%
      select(region = state, price, quantity) %>%
      mutate(quantity = quantity * CONV_MMCF_EJ * 1e6,
             price = (price * gdp_deflator(1975, 2005)) / (CONV_MMCF_EJ * 1e6) ) -> L111.GradeCost.offshore.detailed

    # NOTE:  compute_offshore_costs FUNCTION MUST BE RE-WRITTEN BEFORE THIS CAN BE RE-WRITTEN IN DPLYR
    L111.GradeCost.offshore <- compute_offshore_costs( L111.gas_supply_state_T_EJ_offshore_wide,
                                                       L111.GradeCost.offshore.detailed ) %>%
      mutate(reserve.subresource = "offshore gas")

    # combine all grade extraction cost information together
    L111.GradeCost.gas <-
      bind_rows(L111.GradeCost.onshore,
                L111.GradeCost.offshore,
                L111.GradeCost.onshore.additional_adjusted) %>%
      arrange(region, reserve.subresource, cost) %>%
      clean_grade()


    # Part 3b: oil supply cost (extraction cost by grade)
    # ------------------------------------------------------------------------------------------------------------------------------

    # resource curve
    # First need to specify resource costs
    # TODO: cost simple approximation methods are largely consistent with RIAM, needs a little more description

    # 3.1b) Start with onshore cost estimates
    ETSAP_oil_cost_range %>%
      mutate(reserve.subresource = case_when(
        grepl("conventional", type) ~ "onshore conventional oil",
        T ~ "onshore unconventional oil"
      )) %>%
      # take the average of the different unconventional gas types as the cost for unconventional oil
      group_by(reserve.subresource) %>%
      summarise(low_cost = mean(low_cost),
                high_cost = mean(high_cost)) %>%
      ungroup() %>%
      mutate(grade.hist = low_cost,
             grade.1 = low_cost + 1 * (high_cost - low_cost) / 3,
             grade.2 = low_cost + 2 * (high_cost - low_cost) / 3,
             grade.3 = high_cost) %>%
      gather(grade, cost, -reserve.subresource) %>%
      filter(grepl("grade", grade)) %>%
      mutate(cost = cost * gdp_deflator(1975, 2008)) -> L111.GradeCost.onshore.oil

    # Duplicate costs by state and put on and off shore together
    L111.oil_supply_state_T_EJ_wide %>%
      filter(grepl( "onshore", reserve.subresource )) %>%
      distinct(region) -> onshore_states_oil

    L111.onshore_regions_oil <- unique(onshore_states_oil$region)

    L111.GradeCost.onshore.oil %>%
      repeat_add_columns(tibble("region" = L111.onshore_regions_oil)) %>%
      select(region, reserve.subresource, grade, cost) -> L111.GradeCost.onshore.oil

    # adjust additional cost
    L111.additional_oil_supply_state_EJ %>%
      rename(cost = extractioncost) %>%
      select(region, reserve.subresource, grade, cost) -> L111.GradeCost.onshore.additional.oil

    # shift the cost curve of the additional part higher than the highest cost of ETSAP range
    cost.grade.highest.oil <- L111.GradeCost.onshore.additional.oil %>%
      group_by(reserve.subresource) %>%
      filter(cost == max(cost)) %>%
      ungroup() %>%
      select(reserve.subresource, cost.grade.highest = cost) %>%
      distinct()

    L111.GradeCost.onshore.additional_adjusted.oil <- L111.GradeCost.onshore.additional.oil %>%
      left_join_error_no_match(cost.grade.highest.oil, by = c("reserve.subresource")) %>%
      mutate(cost = cost + cost.grade.highest) %>%
      select(names(L111.GradeCost.onshore.additional.oil))

    # 3.2b) Offshore cost estimates are more detailed however we will just simply these
    # separate OCS quantity by state shares, copying the same price

    BOEM_oil_cost %>%
      # According to BOEM, there is no active offshore lease in Atlantic area since 1984
      # so we don't assume these neighboring states will have offshore gas supply
      # Source: https://www.boem.gov/oil-gas-energy/oil-and-gas-atlantic (last accessed Jan 2023)
      filter(region != "Atlantic OCS") %>%
      mutate(region = case_when(
        region == "Alaska OCS" ~ "AK",
        region == "Pacific OCS" ~ "CA",
        region == "Gulf of Mexico OCS" ~ "GOM"
      )) %>%
      # use the historical production share of GOM states as the proxy for supply
      # use left_join since here just map GOM states
      left_join(offshore_states_oil_GOM_shares %>%
                  group_by(region, state) %>%
                  summarise(share = mean(share)) %>%
                  ungroup, by = "region") %>%
      mutate(state = if_else(is.na(state), region, state),
             share = if_else(is.na(share), 1.0, share)) %>%
      mutate(quantity = quantity * share) %>%
      select(region = state, price, quantity) %>%
      # convert billion barrels of oil into EJ, $2005 into $1975
      mutate(quantity = quantity * CONV_MBBL_EJ * 1e3,
             price = (price * gdp_deflator(1975, 2005)) / (CONV_MMCF_EJ * 1e6) ) -> L111.GradeCost.offshore.detailed.oil

    # NOTE:  compute_offshore_costs CAN BE RE-WRITTEN IN DPLYR
    L111.GradeCost.offshore.oil <- compute_offshore_costs( L111.oil_supply_state_T_EJ_offshore_wide,
                                                           L111.GradeCost.offshore.detailed.oil ) %>%
      mutate(reserve.subresource = "offshore oil")

    # combine all grade extraction cost information together
    L111.GradeCost.oil <-
      bind_rows(L111.GradeCost.onshore.oil,
                L111.GradeCost.offshore.oil,
                L111.GradeCost.onshore.additional_adjusted.oil) %>%
      arrange(region, reserve.subresource, cost) %>%
      clean_grade()


    # Part 3c: coal supply cost (extraction cost by grade)
    # ------------------------------------------------------------------------------------------------------------------------------
    # resource curve
    # First need to specify resource costs
    # TODO: cost simple approximation methods are largely consistent with RIAM, needs a little more description

    # 3.1c) use ETSAP coal cost range
    # use lignite to represent surface mining (lignite are often formed near surface)
    # use hard coal to represent underground mining
    ETSAP_coal_cost_range %>%
      mutate(reserve.subresource = case_when(
        type == "lignite" ~ "surface mining",
        type == "hard coal" ~ "underground mining"
      )) %>%
      # take the average of the different coal mining types
      group_by(reserve.subresource) %>%
      summarise(low_cost = mean(low_cost),
                high_cost = mean(high_cost)) %>%
      ungroup() %>%
      mutate(grade.hist = low_cost,
             grade.1 = low_cost + 1 * (high_cost - low_cost) / 3,
             grade.2 = low_cost + 2 * (high_cost - low_cost) / 3,
             grade.3 = high_cost) %>%
      gather(grade, cost, -reserve.subresource) %>%
      filter(grepl("grade", grade)) %>%
      mutate(cost = cost * gdp_deflator(1975, 2008)) -> L111.GradeCost.coal.etsap

    # Duplicate costs by state
    L111.coal_supply_state_T_EJ_wide %>%
      distinct(region) -> coal_states

    L111.coal_regions <- unique(coal_states$region)

    L111.GradeCost.coal.etsap %>%
      repeat_add_columns(tibble("region" = L111.coal_regions)) %>%
      select(region, reserve.subresource, grade, cost) -> L111.GradeCost.coal.low.grade

    # 3.2c) adjust additional cost
    L111.additional_coal_supply_state_EJ %>%
      rename(cost = extractioncost) %>%
      select(region, reserve.subresource, grade, cost) -> L111.GradeCost.additional.coal

    # shift the cost curve of the additional part higher than the highest cost of ETSAP range
    cost.grade.highest.coal <- L111.GradeCost.additional.coal %>%
      group_by(reserve.subresource) %>%
      filter(cost == max(cost)) %>%
      ungroup() %>%
      select(reserve.subresource, cost.grade.highest = cost) %>%
      distinct()

    L111.GradeCost.additional_adjusted.coal <- L111.GradeCost.additional.coal %>%
      left_join_error_no_match(cost.grade.highest.coal, by = c("reserve.subresource")) %>%
      mutate(cost = cost + cost.grade.highest) %>%
      select(names(L111.GradeCost.additional.coal))

    # 3.3c) combine all grade extraction cost information together
    L111.GradeCost.coal <-
      bind_rows(L111.GradeCost.coal.low.grade,
                L111.GradeCost.additional_adjusted.coal) %>%
      arrange(region, reserve.subresource, cost) %>%
      clean_grade()



    # Part 4: combine availability and extraction cost to construct the supply curve
    # ------------------------------------------------------------------------------------------------------------------------------

    # We need to add resource to cover historical production since it is not included in the supply curves
    # Do not include the first year for cumulative production since the model does not consider it

    # Back calculate reserve additions to be exactly enough given our historical production
    # and assumed production lifetime.  Note production lifetimes may not cover the entire
    # historical period making the calculation a bit more tricky.  We use the lag_prod_helper
    # to help project forward production by each historical vintage so we can take this into
    # account.

    # ------- FOSSIL RESOURCE RESERVE ADDITIONS

    GCAM_timesteps <- diff(MODEL_BASE_YEARS)
    start.year.timestep <- modeltime.PERIOD0_TIMESTEP
    model_year_timesteps <- tibble(year = MODEL_BASE_YEARS, timestep = c(start.year.timestep, GCAM_timesteps))

    # a pipelne helper function to help back calculate new additions to reserve
    # from historical production
    lag_prod_helper <- function(year, value, year_operate, final_year) {
      ret <- value
      for(i in seq_along(year)) {
        if(i == 1) {
          # first year assume all production in this vintage
          ret[i] <- value[i]
        } else if( year_operate[i] > final_year[i]) {
          if(year_operate[i -1] >= final_year[i]) {
            # retired
            ret[i] <- 0
          } else {
            # final timestep that is operating so we must adjust the production
            # by the number of years into the timestep it should have operated
            # incase lifetime and timesteps do not neatly overlap
            ret[i] <- ret[i - 1] * (year_operate[i] - final_year[i]) / (year_operate[i] - year_operate[i-1])
          }
        } else if(year_operate[i] > year[i]) {
          # assume a vintage that as already invested continues at full
          # capacity
          ret[i] <- ret[i -1]
        } else {
          # to determine new investment we take the difference between
          # what the total should be and subtract off production from
          # previous vintages that are still operating
          ret[i] <- 0
          ret[i] <- pmax(value[i] - sum(ret[year_operate == year[i]]), 0)
        }
      }
      ret
    }

    # 4a) gas
    L111.gas_production_states_EJ %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(select(A10.ResSubresourceProdLifetime, resource, lifetime = avg.prod.lifetime),
                               by=c("resource")) %>%
      left_join_error_no_match(model_year_timesteps, by = c("year")) %>%
      repeat_add_columns(tibble(year_operate = MODEL_BASE_YEARS)) %>%
      mutate(final_year = pmin(MODEL_BASE_YEARS[length(MODEL_BASE_YEARS)], (year - timestep + lifetime))) %>%
      filter(year_operate >= year - timestep + 1) %>%
      group_by(region, resource, reserve.subresource) %>%
      mutate(value = lag_prod_helper(year, value, year_operate, final_year)) %>%
      ungroup() %>%
      filter(year == year_operate) %>%
      mutate(value = value * lifetime) %>%
      mutate(grade = "grade.hist") %>%
      group_by(region, resource, reserve.subresource, grade) %>%
      summarise(available = sum(value)) %>%
      ungroup() -> L111.CumulHistProduction.gas

    # Merge costs and available
    # Sort by costs while grouping by state and resource to get grades in an appropriate order
    L111.gas_supply_state_T_EJ_wide %>%
      gather(grade, available, -region, -resource, -reserve.subresource) -> L111.GradeAvail.gas

    L111.additional_gas_supply_state_EJ %>%
      select(names(L111.GradeAvail.gas)) -> L111.GradeAvail.additional.gas

    L111.ResCurve.gas <- bind_rows(L111.CumulHistProduction.gas, L111.GradeAvail.gas, L111.GradeAvail.additional.gas) %>%
      clean_grade() %>%
      left_join_error_no_match(L111.GradeCost.gas, by = c("region", "reserve.subresource", "grade")) %>%
      mutate(available = round(available, 7),
             extractioncost = round(cost, 3)) %>%
      select(region, resource, reserve.subresource, grade, available, extractioncost) %>%
      arrange(region, resource, reserve.subresource, extractioncost) %>%
      # keep resource curve for states with historical productions (filter WA out)
      # TODO: check whether this is reasonable
      # However, there are also three states with historical productions "MD, SD, and TN" but no resource curve
      filter(region %in% unique(L111.gas_production_states_EJ$region))

    # 4b) oil
    L111.oil_production_states_EJ %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      # regroup resource type to be consistent with GCAM core model naming in order to match lifetime
      mutate(resource_type = ifelse(reserve.subresource == "onshore conventional oil", "crude oil", "unconventional oil")) %>%
      left_join_error_no_match(select(A10.ResSubresourceProdLifetime, resource_type = reserve.subresource, lifetime = avg.prod.lifetime),
                               by=c("resource_type")) %>%
      select(-resource_type) %>%
      left_join_error_no_match(model_year_timesteps, by = c("year")) %>%
      repeat_add_columns(tibble(year_operate = MODEL_BASE_YEARS)) %>%
      mutate(final_year = pmin(MODEL_BASE_YEARS[length(MODEL_BASE_YEARS)], (year - timestep + lifetime))) %>%
      filter(year_operate >= year - timestep + 1) %>%
      group_by(region, resource, reserve.subresource) %>%
      mutate(value = lag_prod_helper(year, value, year_operate, final_year)) %>%
      ungroup() %>%
      filter(year == year_operate) %>%
      mutate(value = value * lifetime) %>%
      mutate(grade = "grade.hist") %>%
      group_by(region, resource, reserve.subresource, grade) %>%
      summarise(available = sum(value)) %>%
      ungroup() -> L111.CumulHistProduction.oil

    # Merge costs and available
    # Sort by costs while grouping by state and resource to get grades in an appropriate order
    L111.oil_supply_state_T_EJ_wide %>%
      gather(grade, available, -region, -resource, -reserve.subresource) -> L111.GradeAvail.oil

    L111.additional_oil_supply_state_EJ %>%
      select(names(L111.GradeAvail.oil)) -> L111.GradeAvail.additional.oil

    L111.ResCurve.oil <- bind_rows(L111.CumulHistProduction.oil, L111.GradeAvail.oil, L111.GradeAvail.additional.oil) %>%
      clean_grade() %>%
      left_join_error_no_match(L111.GradeCost.oil, by = c("region", "reserve.subresource", "grade")) %>%
      mutate(available = round(available, 7),
             extractioncost = round(cost, 3)) %>%
      select(region, resource, reserve.subresource, grade, available, extractioncost) %>%
      arrange(region, resource, reserve.subresource, extractioncost) %>%
      # keep resource curve for states with historical productions
      filter(region %in% unique(L111.oil_production_states_EJ$region))

    # 4c) coal
    L111.coal_production_states_EJ %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(select(A10.ResSubresourceProdLifetime, resource, lifetime = avg.prod.lifetime),
                               by=c("resource")) %>%
      left_join_error_no_match(model_year_timesteps, by = c("year")) %>%
      repeat_add_columns(tibble(year_operate = MODEL_BASE_YEARS)) %>%
      mutate(final_year = pmin(MODEL_BASE_YEARS[length(MODEL_BASE_YEARS)], (year - timestep + lifetime))) %>%
      filter(year_operate >= year - timestep + 1) %>%
      group_by(region, resource, reserve.subresource) %>%
      mutate(value = lag_prod_helper(year, value, year_operate, final_year)) %>%
      ungroup() %>%
      filter(year == year_operate) %>%
      mutate(value = value * lifetime) %>%
      mutate(grade = "grade.hist") %>%
      group_by(region, resource, reserve.subresource, grade) %>%
      summarise(available = sum(value)) %>%
      ungroup() -> L111.CumulHistProduction.coal

    # Merge costs and available
    # Sort by costs while grouping by state and resource to get grades in an appropriate order
    L111.coal_supply_state_T_EJ_wide %>%
      gather(grade, available, -region, -resource, -reserve.subresource) -> L111.GradeAvail.coal

    L111.additional_coal_supply_state_EJ %>%
      select(names(L111.GradeAvail.coal)) -> L111.GradeAvail.additional.coal

    L111.ResCurve.coal <- bind_rows(L111.CumulHistProduction.coal, L111.GradeAvail.coal, L111.GradeAvail.additional.coal) %>%
      clean_grade() %>%
      # just keep coal producing states
      filter(region %in% L111.GradeCost.coal$region) %>%
      left_join_error_no_match(L111.GradeCost.coal, by = c("region", "reserve.subresource", "grade")) %>%
      mutate(available = round(available, 7),
             extractioncost = round(cost, 3)) %>%
      select(region, resource, reserve.subresource, grade, available, extractioncost) %>%
      arrange(region, resource, reserve.subresource, extractioncost) %>%
      # keep resource curve for states with historical productions
      filter(region %in% unique(L111.coal_production_states_EJ$region))


    # Part 5: combine all resources into the same table
    # ------------------------------------------------------------------------------------------------------------------------------
    # extra filter to make sure keep coal to only coal-producing states
    L111.coal_production_states_EJ <- L111.coal_production_states_EJ %>% filter(region %in% unique(L111.ResCurve.coal$region))

    L111.Prod_EJ_R_F_Yh_USA <- bind_rows(L111.gas_production_states_EJ, L111.oil_production_states_EJ, L111.coal_production_states_EJ)

    # combine all resource curve first
    L111.ResCurves_EJ_R_Ffos_USA_combined <- bind_rows(L111.ResCurve.gas, L111.ResCurve.oil, L111.ResCurve.coal)

    # create a "highest grade" with zero available
    L111.ResCurves_EJ_R_Ffos_USA_highest <- L111.ResCurves_EJ_R_Ffos_USA_combined %>%
      filter(grade != "grade.hist") %>%
      group_by(region, resource, reserve.subresource) %>%
      slice(n()) %>%
      ungroup() %>%
      # increase to the next grade
      mutate(grade = paste0("grade.", 1 + as.integer(gsub("grade.", "", grade)))) %>%
      # make cost slightly higher (e.g., 10%) and zero available
      mutate(available = 0, extractioncost = extractioncost * 1.1)

    # combine with the original curve
    L111.ResCurves_EJ_R_Ffos_USA <- bind_rows(L111.ResCurves_EJ_R_Ffos_USA_combined,
                                              L111.ResCurves_EJ_R_Ffos_USA_highest) %>%
      arrange(region, resource, reserve.subresource)

    # ===================================================
    # Produce outputs

    L111.ResCurves_EJ_R_Ffos_USA %>%
      add_title("Natural gas and oil supply and extraction cost by state and resource type") %>%
      add_units("EJ") %>%
      add_comments("Natural gas supply by state and resource type") %>%
      add_precursors("gcam-usa/USGS_gas_supply_Quad",
                     "gcam-usa/USGS_oil_supply_Quad",
                     "gcam-usa/USGS_basin_state_mapping",
                     "gcam-usa/BOEM_gas_supply_EJ",
                     "gcam-usa/BOEM_oil_supply_EJ",
                     "gcam-usa/EIA_coal_reserve_2021_Mton",
                     "gcam-usa/ETSAP_gas_cost_range",
                     "gcam-usa/ETSAP_oil_cost_range",
                     "gcam-usa/ETSAP_coal_cost_range",
                     "gcam-usa/BOEM_gas_cost",
                     "gcam-usa/BOEM_oil_cost",
                     "energy/A10.ResSubresourceProdLifetime",
                     "L111.RsrcCurves_EJ_R_Ffos") ->
      L111.ResCurves_EJ_R_Ffos_USA

    L111.Prod_EJ_R_F_Yh_USA %>%
      add_title("Downscaled to state US natural gas and oil primary production by resource type / year") %>%
      add_units("EJ") %>%
      add_comments("Downscaled to state US natural gas primary production by resource type / year") %>%
      add_precursors("gcam-usa/EIA_gas_market_prod_state_MMcf_total",
                     "gcam-usa/EIA_gas_market_prod_state_Bcf_coalbed",
                     "gcam-usa/EIA_gas_market_prod_state_Bcf_shalegas",
                     "gcam-usa/EIA_NG_prod_mapping_total",
                     "gcam-usa/EIA_NG_prod_mapping_coalbed",
                     "gcam-usa/EIA_NG_prod_mapping_shalegas",
                     "gcam-usa/EIA_oil_market_prod_state_thousBBL_total",
                     "gcam-usa/EIA_tight_oil_production_mbbl_per_day",
                     "gcam-usa/EIA_tight_oil_play_state_mapping",
                     "gcam-usa/EIA_oil_GOM_refineries_input_2022",
                     "gcam-usa/Alaska_offshore_gas_oil_2021",
                     "gcam-usa/EIA_coal_prod_state_ton_surface_2001_2021",
                     "gcam-usa/EIA_coal_prod_state_ton_underground_2001_2021",
                     "L111.Prod_EJ_R_F_Yh") ->
      L111.Prod_EJ_R_F_Yh_USA

    return_data(L111.ResCurves_EJ_R_Ffos_USA,
                L111.Prod_EJ_R_F_Yh_USA)

  } else {
    stop("Unknown command")
  }
}
