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
             FILE = "gcam-usa/USGS_basin_state_mapping",
             FILE = "gcam-usa/BOEM_gas_supply_EJ",
             FILE = "gcam-usa/ETSAP_gas_cost_range",
             FILE = "gcam-usa/BOEM_gas_cost",
             FILE = "gcam-usa/EIA_gas_market_prod_state_MMcf_total",
             FILE = "gcam-usa/EIA_gas_market_prod_state_Bcf_coalbed",
             FILE = "gcam-usa/EIA_gas_market_prod_state_Bcf_shalegas",
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
    USGS_basin_state_mapping <- get_data(all_data, "gcam-usa/USGS_basin_state_mapping")
    BOEM_gas_supply_EJ <- get_data(all_data, "gcam-usa/BOEM_gas_supply_EJ")
    ETSAP_gas_cost_range <- get_data(all_data, "gcam-usa/ETSAP_gas_cost_range", strip_attributes = TRUE)
    BOEM_gas_cost <- get_data(all_data, "gcam-usa/BOEM_gas_cost", strip_attributes = TRUE)
    EIA_gas_market_prod_state_MMcf_total <- get_data(all_data, "gcam-usa/EIA_gas_market_prod_state_MMcf_total")
    EIA_gas_market_prod_state_Bcf_coalbed <- get_data(all_data, "gcam-usa/EIA_gas_market_prod_state_Bcf_coalbed")
    EIA_gas_market_prod_state_Bcf_shalegas <- get_data(all_data, "gcam-usa/EIA_gas_market_prod_state_Bcf_shalegas")
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
        ret.costs.region$cost[4] <- ret.costs.region$cost[3] * 0.6
        ret.costs <- rbind( ret.costs, ret.costs.region )
      }
      return(ret.costs)
    }

    # Part 1: historical production
    # ------------------------------------------------------------------------------------------------------------------------------
    # clean up data
    # Federal offshore are aggregated into the nearest states
    # For single state: Federal offshore just combine with state offshore
    # for Federal Gulf of Mexico, these data are available between 1997-2021, just allocate them into the nearest three
    # Federal offshore states (data only available between 1992-1998): based on their average production share during  1991-1998

    # unconventional gas includes coalbed methane and shale gas, and their productions are separated reported by EIA
    # conventional gas is obtained by subtracting the unconventional gas from total onshore gas

    # 1.1 Split production into onshore and offshore

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
      mutate(type = ifelse(type == "total", "onshore", type))

    # 1.2 Split onshore into conventional and unconventional

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
      mutate(value = ifelse(value < 0, 0, value))

    # 1.3 clean up offshore production

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

    # 1.4 combine onshore (conventional and unconventional) and offshore
    L111.gas_production_states_EJ_EIA <- L111.gas_production_onshore %>%
      bind_rows(L111.gas_production_offshore) %>%
      rename(region = state) %>%
      mutate(resource = "natural gas",
             reserve.subresource = paste0(type, " gas")) %>%
      group_by(region, resource, reserve.subresource, year) %>%
      summarise(eia.prod = sum(value)) %>%
      ungroup()

    # 1.5 use EIA produciton ratio to downscale national total values
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


    # Part 2: gas supply (supply by grade)
    # ------------------------------------------------------------------------------------------------------------------------------

    # Map supply from basins to states.
    L111.gas_prob_names <- names( USGS_gas_supply_Quad )[ grep('^F\\d\\d$', names( USGS_gas_supply_Quad ), perl=TRUE ) ]

    USGS_basin_state_mapping %>%
      # number of rows will change since one basin could be mapped to multiple states
      left_join(USGS_gas_supply_Quad, by = c("basin")) %>%
      gather(type, value, -state, -basin, -fraction, -row, -resource) %>%
      mutate(value = value * fraction * CONV_BTU_KJ ) %>%
      # aggregate all types into the oneshore natural gas
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
      mutate(state = ifelse(is.na(state), region, state),
             GOM_share = ifelse(is.na(GOM_share), 1.0, GOM_share)) %>%
      mutate(value = value * GOM_share) %>%
      mutate(resource = "natural gas",
             reserve.subresource = "offshore gas") %>%
      select(region = state, resource, reserve.subresource, grade, value) %>%
      spread(grade, value)

    # combine with onshore and offshore gas supply
    L111.gas_supply_state_T_EJ_onshore_wide %>%
      bind_rows(L111.gas_supply_state_T_EJ_offshore_wide) -> L111.gas_supply_state_T_EJ_wide

    # harmonize with the national supply
    # 1) create state shares from USGS data
    L111.gas_supply_state_T_EJ_wide %>%
      mutate(total = grade.1 + grade.2 + grade.3) %>%
      group_by(region) %>%
      summarise(total = sum(total)) %>%
      mutate(region_sum = sum(total)) %>%
      mutate(share = total / region_sum) %>%
      select(-total, -region_sum) -> L111.additional_gas_supply_downscale

    # 2) sum all natural gas resources to national total (USGS and BOEM data).
    L111.gas_supply_state_T_EJ_wide %>%
      mutate(GCAM_region_ID = gcamusa.USA_REGION_NUMBER) %>%
      group_by(GCAM_region_ID) %>%
      summarise(USGS_total = sum(grade.1, grade.2, grade.3)) %>%
      ungroup() -> L111.gas_supply_USA_USGS_BOEM_T_EJ

    # 3) Subtract from energy data system natural gas resource curve (L111.RsrcCurves_EJ_R_Ffos)
    # to get remaining natural gas resources.
    L111.RsrcCurves_EJ_R_Ffos %>%
      filter(GCAM_region_ID == gcamusa.USA_REGION_NUMBER) %>%
      filter(resource == "natural gas") %>%
      mutate(grade = as.numeric(gsub("grade", "", grade))) %>%
      mutate(cum_avail = cumsum(available)) %>%
      left_join_error_no_match(L111.gas_supply_USA_USGS_BOEM_T_EJ, by = c("GCAM_region_ID")) %>%
      mutate(cum_avail_remain = cum_avail - USGS_total) %>%
      filter(cum_avail_remain >= 0) %>%
      # reset the starting point of the lowest grade as the cumulaitve remaining of "additional gas"
      mutate(available = ifelse(grade == min(grade), cum_avail_remain, available)) %>%
      select(-cum_avail, -USGS_total, -cum_avail_remain) %>%
      mutate(grade = paste("grade", grade, sep = ' ')) ->
      L111.RsrcCurves_EJ_R_gas_additional

    # Apply state shares to remaining natural gas resources
    additional_gas_supply_states <- distinct(L111.additional_gas_supply_downscale %>% select(region))

    L111.RsrcCurves_EJ_R_gas_additional %>%
      repeat_add_columns(additional_gas_supply_states) %>%
      left_join_error_no_match(L111.additional_gas_supply_downscale, by = c("region")) %>%
      mutate(available = available * share) %>%
      mutate(resource = "natural gas",
             # assuming additional supply are conventional gas
             reserve.subresource = "onshore conventional gas") %>%
      select(region, resource, reserve.subresource, grade, extractioncost, available) -> L111.additional_gas_supply_state_EJ


    # Part 3: gas supply cost (extraction cost by grade)
    # ------------------------------------------------------------------------------------------------------------------------------

    # resource curve
    # First need to specify resource costs
    # TODO: cost simple approximation methods are largely consistent with RIAM, needs a little more description

    # 1) Start with onshore cost estimates
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

    # since L111.GradeCost.onshore.additional started in grade 4, so logically their extraction.cost should be
    # higher than the extraction cost of grade 3
    # for text, just shift the cost curve upward by the magnitute of grade.3's cost
    L111.GradeCost.onshore.additional_adjusted <- L111.GradeCost.onshore.additional %>%
      mutate(cost.grade.3 = max(L111.GradeCost.onshore$cost)) %>%
      mutate(cost = cost + cost.grade.3) %>%
      select(names(L111.GradeCost.onshore.additional))

    # 2) Offshore cost estimates are more detailed however we will just simply these
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
      mutate(state = ifelse(is.na(state), region, state),
             GOM_share = ifelse(is.na(GOM_share), 1.0, GOM_share)) %>%
      mutate(quantity = quantity * GOM_share) %>%
      select(region = state, price, quantity) %>%
      mutate(quantity = quantity * CONV_MMCF_EJ * 1e6,
             price = (price * gdp_deflator(1975, 2005)) / (CONV_MMCF_EJ * 1e6) ) -> L111.GradeCost.offshore.detailed

    # NOTE:  compute_offshore_costs FUNCTION MUST BE RE-WRITTEN BEFORE THIS CAN BE RE-WRITTEN IN DPLYR
    L111.GradeCost.offshore <- compute_offshore_costs( L111.gas_supply_state_T_EJ_offshore_wide,
                                                       L111.GradeCost.offshore.detailed ) %>%
      mutate(reserve.subresource = "offshore gas")

    # combine all grade extraction cost information together
    L111.GradeCost <-
      bind_rows(L111.GradeCost.onshore,
                L111.GradeCost.offshore,
                L111.GradeCost.onshore.additional_adjusted) %>%
      arrange(region, reserve.subresource, cost)


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
      ungroup() -> L111.CumulmHistProduction

    # Merge costs and available
    # Sort by costs while grouping by state and resource to get grades in an appropriate order
    L111.gas_supply_state_T_EJ_wide %>%
      gather(grade, available, -region, -resource, -reserve.subresource) -> L111.GradeAvail

    L111.additional_gas_supply_state_EJ %>%
      select(names(L111.GradeAvail)) -> L111.GradeAvail.additional

    L111.ResCurve <- bind_rows(L111.CumulHistProduction, L111.GradeAvail, L111.GradeAvail.additional) %>%
      left_join_error_no_match(L111.GradeCost, by = c("region", "reserve.subresource", "grade")) %>%
      mutate(available = round(available, 7),
             extractioncost = round(cost, 3)) %>%
      select(region, resource, reserve.subresource, grade, available, extractioncost) %>%
      arrange(region, resource, reserve.subresource, extractioncost) %>%
      # keep resource curve for states with historical productions (filter WA out)
      # TODO: check whether this is reasonable
      # However, there are also three states with historical productions "MD, SD, and TN" but no resource curve
      filter(region %in% unique(L111.gas_production_states_EJ$region))

    # ===================================================
    # Produce outputs

    L111.ResCurve %>%
      add_title("Natural gas supply and extraction cost by state and resource type") %>%
      add_units("EJ") %>%
      add_comments("Natural gas supply by state and resource type") %>%
      add_precursors("gcam-usa/USGS_gas_supply_Quad",
                     "gcam-usa/USGS_basin_state_mapping",
                     "gcam-usa/BOEM_gas_supply_EJ",
                     "gcam-usa/ETSAP_gas_cost_range",
                     "gcam-usa/BOEM_gas_cost",
                     "energy/A10.ResSubresourceProdLifetime",
                     "L111.RsrcCurves_EJ_R_Ffos") ->
      L111.ResCurves_EJ_R_Ffos_USA

    L111.gas_production_states_EJ %>%
      add_title("Downscaled to state US natural gas primary production by resource type / year") %>%
      add_units("EJ") %>%
      add_comments("Downscaled to state US natural gas primary production by resource type / year") %>%
      add_precursors("gcam-usa/EIA_gas_market_prod_state_MMcf_total",
                     "gcam-usa/EIA_gas_market_prod_state_Bcf_coalbed",
                     "gcam-usa/EIA_gas_market_prod_state_Bcf_shalegas",
                     "gcam-usa/EIA_NG_prod_mapping_total",
                     "gcam-usa/EIA_NG_prod_mapping_coalbed",
                     "gcam-usa/EIA_NG_prod_mapping_shalegas",
                     "L111.Prod_EJ_R_F_Yh") ->
      L111.Prod_EJ_R_F_Yh_USA

    return_data(L111.ResCurves_EJ_R_Ffos_USA,
                L111.Prod_EJ_R_F_Yh_USA)
  } else {
    stop("Unknown command")
  }
}
