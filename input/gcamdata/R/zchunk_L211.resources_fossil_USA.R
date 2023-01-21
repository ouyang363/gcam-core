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
    return(c(FILE = "gcam-usa/A23.gas_sector_vertical",
             FILE = "energy/A10.TechChange",
             FILE = "gcam-usa/A10.subsector_interp",
             "L111.ResCurves_EJ_R_Ffos_USA",
             "L111.Prod_EJ_R_F_Yh_USA",
             "L210.RsrcPrice",
             "L210.ResSubresourceProdLifetime",
             "L210.SubresourcePriceAdder",
             "L210.ResReserveTechLifetime",
             "L210.ResReserveTechDeclinePhase",
             "L210.ResReserveTechProfitShutdown"))
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
    A23.gas_sector_vertical <- get_data(all_data, "gcam-usa/A23.gas_sector_vertical", strip_attributes = TRUE)
    A10.TechChange <- get_data(all_data, "energy/A10.TechChange", strip_attributes = TRUE)
    A10.subsector_interp <- get_data(all_data, "gcam-usa/A10.subsector_interp", strip_attributes = TRUE)
    L111.ResCurves_EJ_R_Ffos_USA <- get_data(all_data, "L111.ResCurves_EJ_R_Ffos_USA", strip_attributes = TRUE)
    L111.Prod_EJ_R_F_Yh_USA <- get_data(all_data, "L111.Prod_EJ_R_F_Yh_USA", strip_attributes = TRUE)
    L210.RsrcPrice <- get_data(all_data, "L210.RsrcPrice", strip_attributes = TRUE)
    L210.ResSubresourceProdLifetime <- get_data(all_data, "L210.ResSubresourceProdLifetime", strip_attributes = TRUE)
    L210.SubresourcePriceAdder <- get_data(all_data, "L210.SubresourcePriceAdder", strip_attributes = TRUE)
    L210.ResReserveTechLifetime <- get_data(all_data, "L210.ResReserveTechLifetime", strip_attributes = TRUE)
    L210.ResReserveTechDeclinePhase <- get_data(all_data, "L210.ResReserveTechDeclinePhase", strip_attributes = TRUE)
    L210.ResReserveTechProfitShutdown <- get_data(all_data, "L210.ResReserveTechProfitShutdown", strip_attributes = TRUE)

    # ===================================================
    # Perform computations

    # Part 1: construct XML structures for state-level resources
    # -------------------------------------------------------------------------------------------

    # Structure for "natural gas production" supply sector
    L211.gas_prod_sector_name <- "natural gas production"

    A23.gas_sector_vertical %>%
      filter(supplysector == L211.gas_prod_sector_name) %>%
      mutate(region = "USA",
             output.unit="EJ",
             input.unit="EJ",
             price.unit="1975$/GJ",
             logit.year.fillout=MODEL_BASE_YEARS[1],
             logit.type=NA) %>%
      select(region, supplysector, output.unit, input.unit, price.unit,
             logit.year.fillout, logit.exponent, logit.type) -> L211.Supplysector_NGprod


    # L211.Rsrc_F_USA: fossil resource parameters
    L111.Prod_EJ_R_F_Yh_USA %>%
      select(region, resource) %>%
      distinct() %>%
      mutate(output.unit = "EJ",
             price.unit = "1975$/GJ",
             market = region) -> L211.Rsrc_F_USA


    # L211.RsrcPrice_F_USA
    # copy USA historical resource price to all states
    L111.Prod_EJ_R_F_Yh_USA %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      select(region, resource, year) %>%
      distinct() %>%
      left_join_error_no_match(L210.RsrcPrice %>%
                                 filter(region == "USA" & resource == "natural gas") %>%
                                 select(year, price), by = "year") -> L211.RsrcPrice_F_USA

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


    # Part 2: Create state regional natural gas sectors which aggregate resource types.
    # -------------------------------------------------------------------------------------------
    # 1) reserve.subresource lifetime - follow national assumption
    L211.RsrcCalProd_USA %>%
      select(region, resource, reserve.subresource) %>%
      distinct() %>%
      left_join_error_no_match(L210.ResSubresourceProdLifetime %>%
                                 filter(region == "USA" & resource == "natural gas") %>%
                                 select(resource, avg.prod.lifetime), by = "resource") -> L211.ResSubresourceProdLifetime_USA

    # 2) reserve.subresource price adder in 2100 - follow national assumption
    L211.RsrcCalProd_USA %>%
      select(region, resource, reserve.subresource) %>%
      distinct() %>%
      left_join_error_no_match(L210.SubresourcePriceAdder %>%
                                 filter(region == "USA" & resource == "natural gas") %>%
                                 select(resource, year, price.adder), by = "resource") -> L211.SubresourcePriceAdder_USA

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
      # use left_join becuase number of rows will be changes (copy to all years)
      left_join(L210.ResReserveTechLifetime %>%
                  filter(region == "USA" & resource == "natural gas") %>%
                  select(resource, resource.reserve.technology, year, lifetime),
                by = "resource") -> L211.ResReserveTechLifetime_USA

    # 5) reserve.resource.technology decline.phase.percent - follow national assumption
    L211.RsrcCalProd_USA %>%
      select(region, resource, reserve.subresource) %>%
      distinct() %>%
      left_join(L210.ResReserveTechDeclinePhase %>%
                  filter(region == "USA" & resource == "natural gas") %>%
                  select(resource, resource.reserve.technology, year, decline.phase.percent),
                by = "resource") -> L211.ResReserveTechDeclinePhase_USA

    # 6) reserve.resource.technology profit.shutdown parameters - follow national assumption
    L211.RsrcCalProd_USA %>%
      select(region, resource, reserve.subresource) %>%
      distinct() %>%
      left_join(L210.ResReserveTechProfitShutdown %>%
                  filter(region == "USA" & resource == "natural gas") %>%
                  select(resource, resource.reserve.technology, year, median.shutdown.point, profit.shutdown.steepness),
                by = "resource") -> L211.ResReserveTechProfitShutdown_USA

    # Part 3: Create state regional natural gas sectors which aggregate resource types.
    # -------------------------------------------------------------------------------------------

    # L211.Sector: Regional natural gas sector to aggregate gas types.
    L111.Prod_EJ_R_F_Yh_USA %>%
      distinct(region) -> L211.gas_regions

    # Note: these logit assumptions matter
    L211.gas_regions %>%
      mutate(supplysector = L211.gas_prod_sector_name,
             output.unit = "EJ",
             input.unit = "EJ",
             price.unit = "1975$/GJ",
             logit.year.fillout = MODEL_BASE_YEARS[1],
             logit.exponent = -3,
             logit.type = NA) -> L211.Sector

    L211.Supplysector_NGprod %>% bind_rows(L211.Sector) %>%
      select(LEVEL2_DATA_NAMES[["Supplysector"]]) %>%
      mutate(logit.type = NA) -> L211.Sector_prod_USA

    # Add USA & state natural gas production sectors to CO2 Coefs, format for output
    # TODO: will add this to Ccoef files later
    L211.Sector_prod_USA %>%
      select(region) %>%
      distinct() %>%
      mutate(resource = L211.gas_prod_sector_name,
             co2.coef = 14.2) %>%
      rename(PrimaryFuelCO2Coef.name = resource, PrimaryFuelCO2Coef = co2.coef )-> L211.PrimaryCO2Coef_USA

    # L211.Subsector_prod_USA: Regional natural gas subsector to aggregate gas types
    # NOTE: these logit assumptions do not matter as there is no competition at this nest
    L111.Prod_EJ_R_F_Yh_USA %>%
      select(region, resource) %>%
      distinct(region, resource) %>%
      rename(subsector = resource) %>%
      mutate(supplysector = L211.gas_prod_sector_name,
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
      mutate(supplysector = L211.gas_prod_sector_name,
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
    # "L211.TNGSubsInterp: The interpolation rule for the regions in the traded natural gas sector."
    L211.regional_ng_sector <- "natural gas production"

    L111.Prod_EJ_R_F_Yh_USA %>%
      group_by(region, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() -> L211.gas_prod_state_Yh_EJ

    L211.gas_prod_state_Yh_EJ %>%
      rename(state = region) %>%
      distinct(state) %>%
      mutate(region = "USA",
             supplysector = L211.regional_ng_sector,
             subsector = paste(state, L211.gas_prod_sector_name),
             apply.to = "share-weight",
             from.year = as.character(MODEL_FINAL_BASE_YEAR),
             to.year = as.character(MODEL_YEARS[ length( MODEL_YEARS ) ]),
             interpolation.function="fixed" ) %>%
      select(-state) -> L211.TNGSubsInterp_USA

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
    L211.gas_prod_state_Yh_EJ %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(market.name = region) %>%
      mutate(minicam.energy.input = L211.gas_prod_sector_name,
             region = "USA",
             supplysector = L211.regional_ng_sector,
             subsector = paste(market.name, minicam.energy.input),
             technology = subsector) -> L211.TNGTech

    L211.TNGTech %>%
      mutate(calOutputValue = round(value, 7),
             share.weight.year = year,
             subs.share.weight = if_else(value > 0, 1, 0 ),
             tech.share.weight = subs.share.weight) %>%
      select(LEVEL2_DATA_NAMES[["Production"]]) -> L211.TNGTechProduction

    L211.TNGTech %>%
      mutate(coefficient = 1) %>%
      select(LEVEL2_DATA_NAMES[["TechCoef"]]) -> L211.TNGTechCoef




    # ===================================================
    # Produce outputs

    L211.PrimaryCO2Coef_USA %>%
      add_title("USA and state natural gas production carbon Coefs") %>%
      add_units("kg C per GJ") %>%
      add_comments("USA and state natural gas production carbon Coefs") %>%
      add_precursors("L111.Prod_EJ_R_F_Yh_USA") ->
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
      add_comments("copy global assumptions") %>%
      same_precursors_as("L211.PrimaryCO2Coef") ->
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

    L211.Sector_prod_USA %>%
      add_title("supplysector logit table") %>%
      add_units("unitless") %>%
      add_comments("supplysector logit table") %>%
      add_precursors("gcam-usa/A23.gas_sector_vertical",
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

    L211.TNGTechProduction %>%
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
