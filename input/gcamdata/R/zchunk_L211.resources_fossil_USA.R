# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L211.resources_fossil_USA
#'
#' GCAM-USA fossil resource market information, prices, TechChange parameters, and supply curves.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L210.DeleteRenewRsrc_USArsrc}, \code{L210.DeleteUnlimitRsrc_USArsrc}, \code{L210.RenewRsrc_USA},
#' \code{L210.UnlimitRsrc_USA}, \code{L210.UnlimitRsrcPrice_USA}, \code{L210.SmthRenewRsrcTechChange_USA}, \code{L210.SmthRenewRsrcTechChange_offshore_wind_USA},
#' \code{L210.SmthRenewRsrcCurves_wind_USA}, \code{L210.SmthRenewRsrcCurves_offshore_wind_USA}, \code{L210.GrdRenewRsrcCurves_geo_USA}, \code{L210.GrdRenewRsrcMax_geo_USA},
#' \code{L210.SmthRenewRsrcCurvesGdpElast_roofPV_USA}, \code{L210.DeleteUnlimitRsrc_USAlimestone},
#' \code{L210.UnlimitRsrc_limestone_USA}, \code{L210.UnlimitRsrcPrice_limestone_USA}, \code{L210.ResTechShrwt_USA}. The corresponding file in the
#' original data system was \code{L210.resources_USA.R} (gcam-usa level2).
#' @details GCAM-USA fossil resource market information, prices, TechChange parameters, and supply curves.
#' @importFrom assertthat assert_that
#' @importFrom dplyr anti_join filter if_else group_by lag mutate select summarise bind_rows
#' @author YO Jan 2023

module_gcamusa_L211.resources_fossil_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/A10.fossil_sector_vertical",
             FILE = "emissions/A_PrimaryFuelCCoef",
             FILE = "energy/A10.TechChange",
             FILE = "gcam-usa/A10.subsector_interp",
             FILE = "gcam-usa/EIA_gas_market_wellhead_price_states",
             FILE = "gcam-usa/EIA_NG_prod_mapping_wellhead_price",
             FILE = "gcam-usa/EIA_oil_first_purchase_price_states",
             FILE = "gcam-usa/EIA_oil_first_purchase_price_states_mapping",
             FILE = "gcam-usa/EIA_coal_open_market_price_states",
             FILE = "gcam-usa/EIA_coal_open_market_price_states_mapping",
             "L111.ResCurves_EJ_R_Ffos_USA",
             "L111.Prod_EJ_R_F_Yh_USA",
             "L210.RsrcPrice",
             "L210.ResSubresourceProdLifetime",
             "L210.SubresourcePriceAdder",
             "L210.ResReserveTechLifetime",
             "L210.ResReserveTechDeclinePhase",
             "L210.ResReserveTechProfitShutdown",
             "L210.ResTechShrwt"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L211.PrimaryCO2Coef_USA",
             "L211.Rsrc_F_USA",
             "L211.RsrcPrice_F_USA",
             "L211.RsrcCalProd_USA",
             "L211.RsrcTechChange_USA",
             "L211.RsrcCurves_fos_USA",
             "L211.ResSubresourceProdLifetime_USA",
             "L211.SubresourcePriceAdder_USA",
             "L211.ReserveCalReserve_USA",
             "L211.ResReserveTechLifetime_USA",
             "L211.ResReserveTechDeclinePhase_USA",
             "L211.ResReserveTechProfitShutdown_USA",
             "L211.ResReserveTechShrwt_fossil_USA",
             "L211.Sector_prod_USA",
             "L211.Subsector_prod_USA",
             "L211.SubsShrwtFlt_USA",
             "L211.SubsInterpRule_USA",
             "L211.TechShrwt_USA",
             "L211.TechCoef_USA",
             "L211.TechCal_USA",
             "L211.TNGSubsectorLogit",
             "L211.TNGTechProduction_USA",
             "L211.TNGTechCoef_USA")) #
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence package checks
    # TODO
    Geothermal_Hydrothermal_GWh <- State <- available <- b_exp <- cost_modifier <- curve.exponent <- curve_exponent <-
      extractioncost <- generation <- geothermal <- grade <- grade_share <- maxResource <- maxSubResource <- mid.price <-
      mid_p <- mid_price <- object <- offtake <- offtake_share <- region <- renewresource <- smooth.renewable.subresource <-
      state <- unlimited.resource <- value <- year <- year.fillout <- . <-
      sub.renewable.resource <- subresource <- NULL

    # Load required inputs
    A10.fossil_sector_vertical <- get_data(all_data, "gcam-usa/A10.fossil_sector_vertical", strip_attributes = TRUE)
    A_PrimaryFuelCCoef <- get_data(all_data, "emissions/A_PrimaryFuelCCoef", strip_attributes = TRUE)
    A10.TechChange <- get_data(all_data, "energy/A10.TechChange", strip_attributes = TRUE)
    A10.subsector_interp <- get_data(all_data, "gcam-usa/A10.subsector_interp", strip_attributes = TRUE)
    EIA_gas_market_wellhead_price_states <- get_data(all_data, "gcam-usa/EIA_gas_market_wellhead_price_states", strip_attributes = TRUE)
    EIA_NG_prod_mapping_wellhead_price <- get_data(all_data, "gcam-usa/EIA_NG_prod_mapping_wellhead_price", strip_attributes = TRUE)
    EIA_oil_first_purchase_price_states <- get_data(all_data, "gcam-usa/EIA_oil_first_purchase_price_states", strip_attributes = TRUE)
    EIA_oil_first_purchase_price_states_mapping <- get_data(all_data, "gcam-usa/EIA_oil_first_purchase_price_states_mapping", strip_attributes = TRUE)
    EIA_coal_open_market_price_states <- get_data(all_data, "gcam-usa/EIA_coal_open_market_price_states", strip_attributes = TRUE)
    EIA_coal_open_market_price_states_mapping <- get_data(all_data, "gcam-usa/EIA_coal_open_market_price_states_mapping", strip_attributes = TRUE)
    L111.ResCurves_EJ_R_Ffos_USA <- get_data(all_data, "L111.ResCurves_EJ_R_Ffos_USA", strip_attributes = TRUE)
    L111.Prod_EJ_R_F_Yh_USA <- get_data(all_data, "L111.Prod_EJ_R_F_Yh_USA", strip_attributes = TRUE)
    L210.RsrcPrice <- get_data(all_data, "L210.RsrcPrice", strip_attributes = TRUE)
    L210.ResSubresourceProdLifetime <- get_data(all_data, "L210.ResSubresourceProdLifetime", strip_attributes = TRUE)
    L210.SubresourcePriceAdder <- get_data(all_data, "L210.SubresourcePriceAdder", strip_attributes = TRUE)
    L210.ResReserveTechLifetime <- get_data(all_data, "L210.ResReserveTechLifetime", strip_attributes = TRUE)
    L210.ResReserveTechDeclinePhase <- get_data(all_data, "L210.ResReserveTechDeclinePhase", strip_attributes = TRUE)
    L210.ResReserveTechProfitShutdown <- get_data(all_data, "L210.ResReserveTechProfitShutdown", strip_attributes = TRUE)
    L210.ResTechShrwt <- get_data(all_data, "L210.ResTechShrwt", strip_attributes = TRUE)


    # ===================================================
    # Perform computations

    # Part 1: construct XML structures for state-level resources
    # -------------------------------------------------------------------------------------------

    # Structure for "natural gas production", "crude oil production", and "coal production" supply sectors
    L211.fossil_prod_sector_name <- c("natural gas production", "crude oil production", "coal production")

    A10.fossil_sector_vertical %>%
      filter(supplysector %in% L211.fossil_prod_sector_name) %>%
      mutate(region = "USA",
             output.unit="EJ",
             input.unit="EJ",
             price.unit="1975$/GJ",
             logit.year.fillout=MODEL_BASE_YEARS[1],
             logit.type=NA) %>%
      select(region, supplysector, output.unit, input.unit, price.unit,
             logit.year.fillout, logit.exponent, logit.type) -> L211.Supplysector_FFprod


    # L211.Rsrc_F_USA: fossil resource parameters
    L111.Prod_EJ_R_F_Yh_USA %>%
      select(region, resource) %>%
      distinct() %>%
      mutate(output.unit = "EJ",
             price.unit = "1975$/GJ",
             market = region) -> L211.Rsrc_F_USA


    # L211.RsrcPrice_F_USA
    # scale the current region USA price to states
    # scalars are based on the ratio between states' and US' historical wellhead production prices in EIA
    # for missing years, use historical average

    # 1) clean up region mapping
    # natural gas
    EIA_gas_market_wellhead_price_states_raw <- EIA_gas_market_wellhead_price_states %>%
      gather(category, value, -Date) %>%
      rename(year = Date) %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      left_join_error_no_match(EIA_NG_prod_mapping_wellhead_price, by = "category") %>%
      filter(state != "other") %>%
      select(-category) %>%
      mutate(resource = "natural gas")

    # crude oil
    EIA_oil_first_purchase_price_states_raw <- EIA_oil_first_purchase_price_states %>%
      gather(category, value, -Date) %>%
      rename(year = Date) %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      left_join_error_no_match(EIA_oil_first_purchase_price_states_mapping, by = "category") %>%
      filter(state != "other") %>%
      select(-category) %>%
      mutate(resource = "crude oil")

    # coal
    EIA_coal_open_market_price_raw <- EIA_coal_open_market_price_states %>%
      select(-units, -`source key`) %>%
      mutate(description = gsub("Open market : ", "", description)) %>%
      gather(year, value, -description) %>%
      mutate(year = as.integer(year))

    # fill in missing values for states
    # for states without data (W = Data withheld to prevent disclosure), use regional prices instead
    # for regions without data use national values (this is for Alaska)
    EIA_coal_open_market_price_US <- EIA_coal_open_market_price_raw %>%
      filter(description == "United States") %>%
      select(year, value.us = value)

    # obtain regional values after filling in national values to Pacific NAs
    EIA_coal_open_market_price_region <- EIA_coal_open_market_price_raw %>%
      filter(description %in% unique(EIA_coal_open_market_price_states_mapping$region)) %>%
      left_join_error_no_match(EIA_coal_open_market_price_US, by = "year") %>%
      mutate(value.region = if_else(grepl("--|W", value), value.us, value)) %>%
      select(description, year, value.region)

    # obtain state prices after filling in regional values to NA states
    EIA_coal_open_market_price_states_raw <- EIA_coal_open_market_price_raw %>%
      filter(!description %in% unique(EIA_coal_open_market_price_states_mapping$region)) %>%
      # add state to region mapping
      left_join_error_no_match(EIA_coal_open_market_price_states_mapping, by = "description") %>%
      # map the corresponding regions
      left_join_error_no_match(EIA_coal_open_market_price_region, by = c("region" = "description", "year")) %>%
      # fill in missing values with regional prices
      mutate(value = if_else(grepl("--|W", value), value.region, value)) %>%
      select(year, value, state) %>%
      # add back US to calculate scalars in the next step
      bind_rows(EIA_coal_open_market_price_US %>% mutate(state = "USA") %>% rename(value = value.us)) %>%
      # finally add the resource name
      mutate(resource = "coal", value = as.numeric(value))


    # combine
    EIA_fossil_production_price_raw <- bind_rows(EIA_gas_market_wellhead_price_states_raw,
                                                 EIA_oil_first_purchase_price_states_raw,
                                                 EIA_coal_open_market_price_states_raw)


    # 2) develop state-to-US scalars
    EIA_fossil_production_price_states_scalars <- EIA_fossil_production_price_raw %>%
      filter(state != "USA") %>%
      left_join(EIA_fossil_production_price_raw %>%
                  filter(state == "USA") %>%
                  select(resource, year, value_usa = value),
                by = c("year", "resource")) %>%
      mutate(scalars = value / value_usa) %>%
      group_by(state) %>%
      # becuase different states have different years available, the late available year is no later
      # than 2010, here just use the historical average scalar
      mutate(scalar_average = mean(scalars, na.rm = TRUE)) %>%
      ungroup() %>%
      # for states no value available for any historcal year, just use the USA value (scalar = 1)
      # this is only for the case of Navada
      mutate(scalar_average = ifelse(is.na(scalar_average), 1, scalar_average)) %>%
      select(state, resource, scalar = scalar_average) %>%
      distinct()

    # scale state-level calibrated prices based on historical wellhead prices relative to USA
    L111.Prod_EJ_R_F_Yh_USA %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      select(region, resource, year) %>%
      distinct() %>%
      left_join_error_no_match(L210.RsrcPrice %>%
                                 filter(region == "USA") %>%
                                 select(year, resource, price), by = c("resource", "year")) %>%
      # use left_join because Idaho has historical productions but no wellhead price info available
      left_join(EIA_fossil_production_price_states_scalars, by = c("region" = "state", "resource")) %>%
      # replace NA scalars as 1 (using USA value)
      replace_na(list("scalar" = 1)) %>%
      mutate(price = price * scalar ) %>%
      select(-scalar) -> L211.RsrcPrice_F_USA

    # L211.RsrcTechChange_USA: resource technological change
    L111.Prod_EJ_R_F_Yh_USA %>%
      select(region, resource, reserve.subresource) %>%
      distinct() %>%
      # use core GCAM tech.change assumptions
      # use left_join becuase we need to copy this to different years (so number of rows will change)
      left_join(A10.TechChange %>% gather(year, techChange, -resource, -subresource), by = c("resource")) %>%
      mutate(year.fillout = as.integer(year)) %>%
      select(region, resource, reserve.subresource, year.fillout, techChange) ->
      L211.RsrcTechChange_USA

    # L211.RsrcCurves_fos_USA: resource grades and extraction costs
    # this one has been created in level 1 data
    L211.RsrcCurves_fos_USA <- L111.ResCurves_EJ_R_Ffos_USA

    # L211.DepresourceCal: subresource calproduction
    L111.Prod_EJ_R_F_Yh_USA %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      rename(cal.production = value) %>%
      select(region, resource, reserve.subresource, year, cal.production) -> L211.RsrcCalProd_USA


    # Part 2: Create state regional fossil fuel sectors which aggregate resource types.
    # -------------------------------------------------------------------------------------------
    # 1) reserve.subresource lifetime - follow national assumption
    L211.RsrcCalProd_USA %>%
      select(region, resource, reserve.subresource) %>%
      distinct() %>%
      # create a mapping type to match core GCAM's fossil fuel category
      mutate(type = case_when(
        resource == "natural gas" ~ "natural gas",
        resource == "coal" ~ "coal",
        reserve.subresource == "onshore conventional oil" ~ "crude oil",
        reserve.subresource %in% c("onshore unconventional oil", "offshore oil") ~ "unconventional oil")
      ) %>%
      left_join_error_no_match(L210.ResSubresourceProdLifetime %>%
                                 filter(region == "USA") %>%
                                 select(reserve.subresource, avg.prod.lifetime),
                               by = c("type" = "reserve.subresource")) %>%
      select(-type) -> L211.ResSubresourceProdLifetime_USA

    # 2) reserve.subresource price adder in 2100 - follow national assumption
    L211.RsrcCalProd_USA %>%
      select(region, resource, reserve.subresource) %>%
      distinct() %>%
      # create a mapping type to match core GCAM's fossil fuel category
      mutate(type = case_when(
        resource == "natural gas" ~ "natural gas",
        resource == "coal" ~ "coal",
        reserve.subresource == "onshore conventional oil" ~ "crude oil",
        reserve.subresource %in% c("onshore unconventional oil", "offshore oil") ~ "unconventional oil")
      ) %>%
      left_join_error_no_match(L210.SubresourcePriceAdder %>%
                                 filter(region == "USA") %>%
                                 select(subresource, year, price.adder),
                               by = c("type" = "subresource")) %>%
      select(-type) -> L211.SubresourcePriceAdder_USA

    # 3) historical calculated reserve - follow the same method in L210.resources.R

    # Back calculate reserve additions to be exactly enough given our historical production
    # and assumed production lifetime.  Note production lifetimes may not cover the entire
    # historical period making the calculation a bit more tricky.  We use the lag_prod_helper
    # to help project forward production by each historical vintage so we can take this into
    # account.

    # ------- FOSSIL RESOURCE RESERVE ADDITIONS
    # Kind of a level 1.5 we are going to calculate / update historical energy
    # but the years we choose as the model base years matter

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

    L111.Prod_EJ_R_F_Yh_USA %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(select(L211.ResSubresourceProdLifetime_USA, resource, lifetime = avg.prod.lifetime, reserve.subresource) %>% distinct(),
                               by=c("resource", "reserve.subresource")) %>%
      left_join_error_no_match(model_year_timesteps, by = c("year")) %>%
      repeat_add_columns(tibble(year_operate = MODEL_BASE_YEARS)) %>%
      mutate(final_year = pmin(MODEL_BASE_YEARS[length(MODEL_BASE_YEARS)], (year - timestep + lifetime))) %>%
      filter(year_operate >= year - timestep + 1) %>%
      group_by(region, resource, reserve.subresource) %>%
      mutate(value = lag_prod_helper(year, value, year_operate, final_year)) %>%
      ungroup() %>%
      filter(year == year_operate) %>%
      mutate(value = value * lifetime) %>%
      select(-lifetime, -timestep, -year_operate) ->
      L211.Reserve_EJ_R_F_Yh

    L211.Reserve_EJ_R_F_Yh %>%
      rename(cal.reserve = value) %>%
      select(region, resource, reserve.subresource, year, cal.reserve) ->
      L211.ReserveCalReserve_USA

    # 4) reserve.subresource technology lifetime - follow national assumption
    L211.RsrcCalProd_USA %>%
      select(region, resource, reserve.subresource) %>%
      distinct() %>%
      # create a mapping type to match core GCAM's fossil fuel category
      mutate(type = case_when(
        resource == "natural gas" ~ "natural gas",
        resource == "coal" ~ "coal",
        reserve.subresource == "onshore conventional oil" ~ "crude oil",
        reserve.subresource %in% c("onshore unconventional oil", "offshore oil") ~ "unconventional oil")
      ) %>%
      # use left_join because number of rows will be changes (copy to all years)
      left_join(L210.ResReserveTechLifetime %>%
                  filter(region == "USA") %>%
                  select(reserve.subresource, year, lifetime),
                by = c("type" = "reserve.subresource")) %>%
      mutate(resource.reserve.technology = reserve.subresource) %>%
      select(names(L210.ResReserveTechLifetime)) -> L211.ResReserveTechLifetime_USA

    # 5) reserve.resource.technology decline.phase.percent - follow national assumption
    L211.RsrcCalProd_USA %>%
      select(region, resource, reserve.subresource) %>%
      distinct() %>%
      # create a mapping type to match core GCAM's fossil fuel category
      mutate(type = case_when(
        resource == "natural gas" ~ "natural gas",
        resource == "coal" ~ "coal",
        reserve.subresource == "onshore conventional oil" ~ "crude oil",
        reserve.subresource %in% c("onshore unconventional oil", "offshore oil") ~ "unconventional oil")
      ) %>%
      left_join(L210.ResReserveTechDeclinePhase %>%
                  filter(region == "USA") %>%
                  select(reserve.subresource, year, decline.phase.percent),
                by = c("type" = "reserve.subresource")) %>%
      mutate(resource.reserve.technology = reserve.subresource) %>%
      select(names(L210.ResReserveTechDeclinePhase)) -> L211.ResReserveTechDeclinePhase_USA

    # 6) reserve.resource.technology profit.shutdown parameters - follow national assumption
    L211.RsrcCalProd_USA %>%
      select(region, resource, reserve.subresource) %>%
      distinct() %>%
      # create a mapping type to match core GCAM's fossil fuel category
      mutate(type = case_when(
        resource == "natural gas" ~ "natural gas",
        resource == "coal" ~ "coal",
        reserve.subresource == "onshore conventional oil" ~ "crude oil",
        reserve.subresource %in% c("onshore unconventional oil", "offshore oil") ~ "unconventional oil")
      ) %>%
      left_join(L210.ResReserveTechProfitShutdown %>%
                  filter(region == "USA") %>%
                  select(reserve.subresource, year, median.shutdown.point, profit.shutdown.steepness),
                by = c("type" = "reserve.subresource")) %>%
      mutate(resource.reserve.technology = reserve.subresource) %>%
      select(names(L210.ResReserveTechProfitShutdown)) -> L211.ResReserveTechProfitShutdown_USA

    # 7) reserve.resource.technology share.weight - follow national assumption
    L211.RsrcCalProd_USA %>%
      select(region, resource, reserve.subresource) %>%
      distinct() %>%
      # create a mapping type to match core GCAM's fossil fuel category
      mutate(type = case_when(
        resource == "natural gas" ~ "natural gas",
        resource == "coal" ~ "coal",
        reserve.subresource == "onshore conventional oil" ~ "crude oil",
        reserve.subresource %in% c("onshore unconventional oil", "offshore oil") ~ "unconventional oil")
      ) %>%
      left_join(L210.ResTechShrwt %>%
                  filter(region == "USA") %>%
                  select(subresource, year, share.weight),
                by = c("type" = "subresource")) %>%
      mutate(resource.reserve.technology = reserve.subresource) %>%
      select(region, resource, reserve.subresource, resource.reserve.technology, year, share.weight) -> L211.ResReserveTechShrwt_fossil_USA

    # Part 3: Create state regional natural gas sectors which aggregate resource types.
    # -------------------------------------------------------------------------------------------

    # L211.Sector: Regional natural gas sector to aggregate gas types.
    L111.Prod_EJ_R_F_Yh_USA %>%
      distinct(region, resource) -> L211.Rsrc_F_regions

    # Note: these logit assumptions matter
    L211.Rsrc_F_regions %>%
      mutate(supplysector = paste0(resource, " production"),
             output.unit = "EJ",
             input.unit = "EJ",
             price.unit = "1975$/GJ",
             logit.year.fillout = MODEL_BASE_YEARS[1],
             logit.exponent = -3,
             logit.type = NA) -> L211.Sector

    L211.Supplysector_FFprod %>% bind_rows(L211.Sector) %>%
      select(LEVEL2_DATA_NAMES[["Supplysector"]]) %>%
      mutate(logit.type = NA) -> L211.Sector_prod_USA

    # Add USA & state natural gas production sectors to CO2 Coefs, format for output
    # TODO: will add this to Ccoef files later
    L211.Sector_prod_USA %>%
      select(region, supplysector) %>%
      distinct() %>%
      mutate(PrimaryFuelCO2Coef.name = gsub(" production", "", supplysector)) %>%
      left_join_error_no_match(A_PrimaryFuelCCoef, by = "PrimaryFuelCO2Coef.name") %>%
      select(region, PrimaryFuelCO2Coef.name, PrimaryFuelCO2Coef) -> L211.PrimaryCO2Coef_USA

    # L211.Subsector_prod_USA: Regional natural gas subsector to aggregate gas types
    # NOTE: these logit assumptions do not matter as there is no competition at this nest
    L111.Prod_EJ_R_F_Yh_USA %>%
      select(region, subsector = resource) %>%
      distinct(region, subsector) %>%
      mutate(supplysector = paste0(subsector, " production"),
             logit.year.fillout = MODEL_BASE_YEARS[1],
             logit.exponent=-6,
             logit.type=NA) %>%
      select(region, supplysector, subsector, logit.year.fillout, logit.exponent, logit.type) -> L211.Subsector_prod_USA


    # L211.SubsInterpRule: Regional natural gas subsector interpolation rules
    # Shareweight defaults to zero, which will get reset by tech cal if appropriate
    L211.Subsector_prod_USA %>%
      mutate(year.fillout = MODEL_BASE_YEARS[1],
             share.weight = 0) %>%
      select(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]]) -> L211.SubsShrwtFlt_USA

    # Warning: we should partition this table in two if to.value is not NA
    L211.Subsector_prod_USA %>%
      select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
      left_join(A10.subsector_interp, by = c("supplysector", "subsector")) %>%
      set_years() %>%
      mutate(from.year = as.character(from.year),
             to.year = as.character(to.year)) %>%
      select(LEVEL2_DATA_NAMES[["SubsectorInterp"]]) -> L211.SubsInterpRule_States


    # L211.TechInput: Pass through tech
    L211.Subsector_prod_USA %>%
      distinct(region, supplysector,subsector) %>%
      mutate(technology = subsector) %>%
      repeat_add_columns(tibble("year" = MODEL_YEARS)) -> L211.TechInput

    L211.TechInput %>%
      mutate(share.weight = if_else(year <= MODEL_FINAL_BASE_YEAR, 0, 1)) %>%
      select(LEVEL2_DATA_NAMES[["TechShrwt"]]) -> L211.TechShrwt

    L211.TechInput %>%
      mutate(minicam.energy.input = technology,
             coefficient = 1,
             market.name = region) %>%
      select(LEVEL2_DATA_NAMES[["TechCoef"]]) -> L211.TechCoef

    # L211.TechCal_USA: natural gas resource calibration
    L111.Prod_EJ_R_F_Yh_USA %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      filter(value > 0) %>%
      group_by(region, resource, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      mutate(supplysector = paste0(resource, " production"),
             subsector = resource,
             stub.technology = resource,
             minicam.energy.input = resource,
             calibrated.value = round(value, 7),
             share.weight.year = year,
             subs.share.weight = 1,
             tech.share.weight = 1) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCalInput"]]) -> L211.TechCal_USA

    # Part 4: Add these resources to the traded natural gas sector
    # -------------------------------------------------------------------------------------------
    L111.Prod_EJ_R_F_Yh_USA %>%
      group_by(region, resource, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() -> L211.fossil_prod_state_Yh_EJ

    L211.fossil_prod_state_Yh_EJ %>%
      rename(state = region) %>%
      distinct(state, resource) %>%
      mutate(region = "USA",
             supplysector = paste0(resource, " production"),
             subsector = ifelse(state == "AK", paste(state, supplysector),
                                paste("Lower48", supplysector)),
             apply.to = "share-weight",
             from.year = as.character(MODEL_FINAL_BASE_YEAR),
             to.year = as.character(MODEL_YEARS[ length( MODEL_YEARS ) ]),
             interpolation.function="fixed" ) %>%
      select(-state, -resource) %>%
      distinct() -> L211.TNGSubsInterp_USA

    # "L211.TNGSubsInterp: The interpolation rule for the regions in the traded natural gas sector."
    L211.SubsInterpRule_States %>%
      bind_rows(L211.TNGSubsInterp_USA) -> L211.SubsInterpRule_USA

    # L211.TNGSubsectorLogit: The subsector logits for the regions in the traded natural gas sector.
    # NOTE: these logit assumptions do not matter as there is no competition at this nest
    L211.TNGSubsInterp_USA %>%
      mutate(logit.year.fillout = MODEL_BASE_YEARS[1],
             logit.exponent = -6,
             logit.type = NA) %>%
      select(region, supplysector, subsector, logit.year.fillout, logit.exponent, logit.type) -> L211.TNGSubsectorLogit


    # "L211.TNGTech: create the tech to simply pass through state production"
    L211.fossil_prod_state_Yh_EJ %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(market.name = region) %>%
      mutate(minicam.energy.input = paste0(resource, " production"),
             region = "USA",
             supplysector = minicam.energy.input,
             subsector = ifelse(market.name == "AK", paste(market.name, supplysector),
                                paste("Lower48", supplysector)),
             technology = paste(market.name, supplysector)) -> L211.TNGTech

    L211.TNGTech %>%
      mutate(calOutputValue = round(value, 7),
             share.weight.year = year,
             subs.share.weight = if_else(value > 0, 1, 0 ),
             tech.share.weight = subs.share.weight) %>%
      select(LEVEL2_DATA_NAMES[["Production"]]) -> L211.TNGTechProduction

    # use production share as the share.weight for subsector and technology
    # subsector share will split AK vs. lower 48 markets
    # tech share will split state production within lower 48
    L211.TNGTechProduction_subsector_share <- L211.TNGTechProduction %>%
      group_by(region, supplysector, subsector, year) %>%
      dplyr::summarise(subsector.sum = sum(calOutputValue)) %>%
      ungroup() %>%
      group_by(region, supplysector, year) %>%
      mutate(subsector.share = subsector.sum / sum(subsector.sum)) %>%
      ungroup() %>%
      replace_na(list(subsector.share = 0)) %>%
      select(region, supplysector, subsector, year, subsector.share)

    L211.TNGTechProduction_tech_share <- L211.TNGTechProduction %>%
      group_by(region, supplysector, subsector, year) %>%
      mutate(tech.share = calOutputValue / sum(calOutputValue)) %>%
      ungroup() %>%
      replace_na(list(tech.share = 0)) %>%
      select(region, supplysector, subsector, technology, year, tech.share)

    # update share-weight
    L211.TNGTechProduction_USA <- L211.TNGTechProduction %>%
      left_join_error_no_match(L211.TNGTechProduction_subsector_share,
                               by = c("region", "supplysector", "subsector", "year")) %>%
      left_join_error_no_match(L211.TNGTechProduction_tech_share,
                               by = c("region", "supplysector", "subsector", "technology", "year")) %>%
      mutate(subs.share.weight = subsector.share,
             tech.share.weight = tech.share) %>%
      select(LEVEL2_DATA_NAMES[["Production"]])


    L211.TNGTech %>%
      mutate(coefficient = 1) %>%
      select(LEVEL2_DATA_NAMES[["TechCoef"]]) -> L211.TNGTechCoef

    # ===================================================
    # Produce outputs

    L211.PrimaryCO2Coef_USA %>%
      add_title("USA and state fossil resource production carbon Coefs") %>%
      add_units("kg C per GJ") %>%
      add_comments("USA and state fossil resource production carbon Coefs") %>%
      add_precursors("emissions/A_PrimaryFuelCCoef",
                     "L111.Prod_EJ_R_F_Yh_USA") ->
      L211.PrimaryCO2Coef_USA

    L211.Rsrc_F_USA %>%
      add_title("resource parameters") %>%
      add_units("output unit EJ price unit 1975$ per GJ") %>%
      add_comments("resource parameters") %>%
      same_precursors_as("L211.PrimaryCO2Coef") ->
      L211.Rsrc_F_USA

    L211.RsrcPrice_F_USA %>%
      add_title("add a resource price in base year") %>%
      add_units("TODO") %>%
      add_comments("scale USA price assumptions to each state based on EIA historical wellhead prices") %>%
      same_precursors_as("L211.PrimaryCO2Coef") %>%
      add_precursors("gcam-usa/EIA_gas_market_wellhead_price_states",
                     "gcam-usa/EIA_NG_prod_mapping_wellhead_price",
                     "gcam-usa/EIA_oil_first_purchase_price_states",
                     "gcam-usa/EIA_oil_first_purchase_price_states_mapping",
                     "gcam-usa/EIA_coal_open_market_price_states",
                     "gcam-usa/EIA_coal_open_market_price_states_mapping",
                     "L210.RsrcPrice") ->
      L211.RsrcPrice_F_USA

    L211.RsrcCalProd_USA %>%
      add_title("subresource calibrated production") %>%
      add_units("EJ") %>%
      add_comments("subresource calibrated production") %>%
      add_precursors("L111.Prod_EJ_R_F_Yh_USA") ->
      L211.RsrcCalProd_USA

    L211.RsrcTechChange_USA %>%
      add_title("subresource tech change") %>%
      add_units("unitless") %>%
      add_comments("subresource tech change") %>%
      same_precursors_as("L211.PrimaryCO2Coef") %>%
      add_precursors("energy/A10.TechChange") ->
      L211.RsrcTechChange_USA

    L211.RsrcCurves_fos_USA %>%
      add_title("resource curve") %>%
      add_units("unitless") %>%
      add_comments("resource curve") %>%
      add_precursors("L111.ResCurves_EJ_R_Ffos_USA") ->
      L211.RsrcCurves_fos_USA

    L211.ResSubresourceProdLifetime_USA %>%
      add_title("reserve subresource lifetime") %>%
      add_units("years") %>%
      add_comments("copy global assumptions") %>%
      add_precursors("L111.Prod_EJ_R_F_Yh_USA",
                     "L210.ResSubresourceProdLifetime") ->
      L211.ResSubresourceProdLifetime_USA

    L211.SubresourcePriceAdder_USA %>%
      add_title("reserve subresource price adder in 2100") %>%
      add_units("zero") %>%
      add_comments("copy global assumptions") %>%
      add_precursors("L111.Prod_EJ_R_F_Yh_USA",
                     "L210.SubresourcePriceAdder") ->
      L211.SubresourcePriceAdder_USA

    L211.ReserveCalReserve_USA %>%
      add_title("historical calculated reserve") %>%
      add_units("EJ") %>%
      add_comments("follow the same method in L210.resources") %>%
      add_precursors("L111.Prod_EJ_R_F_Yh_USA") ->
      L211.ReserveCalReserve_USA

    L211.ResReserveTechLifetime_USA %>%
      add_title("reserve subresource technology lifetime") %>%
      add_units("years") %>%
      add_comments("copy global assumptions") %>%
      add_precursors("L111.Prod_EJ_R_F_Yh_USA",
                     "L210.ResReserveTechLifetime") ->
      L211.ResReserveTechLifetime_USA

    L211.ResReserveTechDeclinePhase_USA %>%
      add_title("reserve resource technology decline phase percent") %>%
      add_units("years") %>%
      add_comments("copy global assumptions") %>%
      add_precursors("L111.Prod_EJ_R_F_Yh_USA",
                     "L210.ResReserveTechDeclinePhase") ->
      L211.ResReserveTechDeclinePhase_USA

    L211.ResReserveTechProfitShutdown_USA %>%
      add_title("reserve resource technology profit shutdown parameters") %>%
      add_units("years") %>%
      add_comments("copy global assumptions") %>%
      add_precursors("L111.Prod_EJ_R_F_Yh_USA",
                     "L210.ResReserveTechProfitShutdown") ->
      L211.ResReserveTechProfitShutdown_USA

    L211.ResReserveTechShrwt_fossil_USA %>%
      add_title("reserve resource technology shareweight") %>%
      add_units("years") %>%
      add_comments("copy global assumptions") %>%
      add_precursors("L111.Prod_EJ_R_F_Yh_USA",
                     "L210.ResTechShrwt") ->
      L211.ResReserveTechShrwt_fossil_USA

    L211.Sector_prod_USA %>%
      add_title("supplysector logit table") %>%
      add_units("unitless") %>%
      add_comments("supplysector logit table") %>%
      add_precursors("gcam-usa/A10.fossil_sector_vertical",
                     "L111.Prod_EJ_R_F_Yh_USA") ->
      L211.Sector_prod_USA

    L211.Subsector_prod_USA %>%
      add_title("subresource logit table") %>%
      add_units("unitless") %>%
      add_comments("subresource logit table") %>%
      add_precursors("L111.Prod_EJ_R_F_Yh_USA") ->
      L211.Subsector_prod_USA

    L211.SubsShrwtFlt_USA %>%
      add_title("subsector shareweight table") %>%
      add_units("unitless") %>%
      add_comments("subsector shareweight table") %>%
      same_precursors_as("L211.Subsector_prod_USA") ->
      L211.SubsShrwtFlt_USA

    L211.SubsInterpRule_USA %>%
      add_title("subsector interpolation rule") %>%
      add_units("unitless") %>%
      add_comments("subsector interpolation rule") %>%
      same_precursors_as("L211.Subsector_prod_USA") %>%
      add_precursors("gcam-usa/A10.subsector_interp") ->
      L211.SubsInterpRule_USA

    L211.TechShrwt %>%
      add_title("global technology database technology shareweight") %>%
      add_units("unitless") %>%
      add_comments("global technology database technology shareweight") %>%
      same_precursors_as("L211.Subsector_prod_USA") ->
      L211.TechShrwt_USA

    L211.TechCoef %>%
      add_title("global technology database technology coefficient") %>%
      add_units("unitless") %>%
      add_comments("global technology database technology coefficient") %>%
      same_precursors_as("L211.Subsector_prod_USA") ->
      L211.TechCoef_USA

    L211.TechCal_USA %>%
      add_title("natural gas resource technology calibration") %>%
      add_units("EJ") %>%
      add_comments("natural gas resource technology calibration") %>%
      add_precursors("L111.Prod_EJ_R_F_Yh_USA") ->
      L211.TechCal_USA

    L211.TNGSubsectorLogit %>%
      add_title("create subsector logit for national pass-through natural gas production") %>%
      add_units("unitless") %>%
      add_comments("create subsector logit for national pass-through natural gas production") %>%
      same_precursors_as("L211.SubsInterpRule_USA") ->
      L211.TNGSubsectorLogit

    L211.TNGTechProduction_USA %>%
      add_title("create the tech to simply pass through state production to USA and calibration technology production") %>%
      add_units("unitless") %>%
      add_comments("create the tech to simply pass through state production to USA and calibration technology production") %>%
      add_precursors("L111.Prod_EJ_R_F_Yh_USA") ->
      L211.TNGTechProduction_USA

    L211.TNGTechCoef %>%
      add_title("create the tech to simply pass through state production to USA and set coefficient as 1") %>%
      add_units("unitless") %>%
      add_comments("create the tech to simply pass through state production to USA and set coefficient as 1") %>%
      add_precursors("L111.Prod_EJ_R_F_Yh_USA") ->
      L211.TNGTechCoef_USA

    return_data(L211.PrimaryCO2Coef_USA,
                L211.Rsrc_F_USA,
                L211.RsrcPrice_F_USA,
                L211.RsrcCalProd_USA,
                L211.RsrcTechChange_USA,
                L211.RsrcCurves_fos_USA,
                L211.ResSubresourceProdLifetime_USA,
                L211.SubresourcePriceAdder_USA,
                L211.ReserveCalReserve_USA,
                L211.ResReserveTechLifetime_USA,
                L211.ResReserveTechDeclinePhase_USA,
                L211.ResReserveTechProfitShutdown_USA,
                L211.ResReserveTechShrwt_fossil_USA,
                L211.Sector_prod_USA,
                L211.Subsector_prod_USA,
                L211.SubsShrwtFlt_USA,
                L211.SubsInterpRule_USA,
                L211.TechShrwt_USA,
                L211.TechCoef_USA,
                L211.TechCal_USA,
                L211.TNGSubsectorLogit,
                L211.TNGTechProduction_USA,
                L211.TNGTechCoef_USA)

  } else {
    stop("Unknown command")
  }
}
