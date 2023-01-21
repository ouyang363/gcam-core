# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L2262.regional_fossil_USA
#'
#' Create fossil energy supply sectors at the state level.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2261.DeleteSupplysector_bio_USA}, \code{L2261.Supplysector_bio_USA},
#' \code{L2261.SubsectorShrwtFllt_bio_USA}, \code{L2261.SubsectorInterp_bio_USA}, \code{L2261.SubsectorLogit_bio_USA},
#' \code{L2261.StubTech_bio_USA}, \code{L2261.StubTechMarket_bio_USA}, \code{L2261.StubTechShrwt_rbO_USA},
#' \code{L2261.StubTechFractSecOut_bio_USA}, \code{L2261.StubTechFractProd_bio_USA}, \code{L2261.Rsrc_DDGS_USA},
#' \code{L2261.RsrcPrice_DDGS_USA}, \code{L2261.Tech_rbm_USA}, \code{L2261.TechShrwt_rbm_USA},
#' \code{L2261.TechCoef_rbm_USA}, \code{L2261.Tech_dbm_USA}, \code{L2261.TechShrwt_dbm_USA},
#' \code{L2261.TechEff_dbm_USA}, \code{L2261.TechCost_dbm_USA}, \code{L2261.CarbonCoef_bio_USA},
#' \code{L2261.StubTechMarket_en_USA}, \code{L2261.StubTechMarket_elecS_USA}, \code{L2261.StubTechMarket_ind_USA},
#' \code{L2261.StubTechMarket_cement_USA}, \code{L2261.StubTechMarket_bld_USA}.
#' The corresponding file in the original data system was \code{L2261.regional_biomass_USA.R} (gcam-usa level2).
#' @details Create biomass supply sectors at the state level, in order ensure that biomass carbon-tracking is
#' contained entirely within the consuming region (state).
#' @importFrom assertthat assert_that
#' @importFrom dplyr distinct filter inner_join if_else mutate select semi_join
#' @importFrom tibble tibble
#' @author YO Jan 2023

module_gcamusa_L2262.regional_fossil_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/A23.gas_sector_vertical",
             FILE = "gcam-usa/A23.gas_subsector_shrwt",
             FILE = "gcam-usa/A23.gas_subsector_shrwt_interp",
             FILE = 'gcam-usa/A23.gas_subsector_shrwt_interpto',
             FILE = "gcam-usa/A23.gas_tech_associations",
             "L111.Prod_EJ_R_F_Yh_USA",
             "L222.StubTechProd_gasproc",
             "L239.PrimaryConsKeyword_en"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2262.DeleteRsrc_NG_USA",
             "L2262.Delete_regNG_supplysector",
             "L2262.Supplysector_regNG_USA",
             "L2262.Subsector_regNG_USA",
             "L2262.PrimaryConsKeyword_regNG_USA",
             "L2262.StubTechProd_regNG_USA",
             "L2262.TechCoef_regNG_USA",
             "L2262.TechShrwt_regNG_USA",
             "L2262.SubsectorShrwt_regNG_USA",
             "L2262.SubsectorShrwtInterp_regNG_USA",
             "L2262.SubsectorShrwtInterpTo_regNG_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # silence check package notes
    from.year <- input.cost <- input.unit <- logit.exponent <- logit.type <- market.name <-
      minicam.non.energy.input <- output.unit <- price.unit <- region <- sector.name <- state <-
      stub.technology <- subsector <- supplysector <- technology <- to.year <- year <- traded <-
      subsector.name <- share.weight <- fractional.secondary.output <- price <- fraction.produced <-
      PrimaryFuelCO2Coef.name <- PrimaryFuelCO2Coef <- minicam.energy.input <- sector <- calibrated.value <-
      value <- share <- fuel <- . <- output.ratio <- subs.share.weight <- tech.share.weight <-
      subsector_1 <- to.technology <- NULL

    # Load required inputs
    A23.gas_sector_vertical <- get_data(all_data, "gcam-usa/A23.gas_sector_vertical", strip_attributes = TRUE)
    A23.gas_subsector_shrwt <- get_data(all_data, "gcam-usa/A23.gas_subsector_shrwt", strip_attributes = TRUE)
    A23.gas_subsector_shrwt_interp <- get_data(all_data, "gcam-usa/A23.gas_subsector_shrwt_interp", strip_attributes = TRUE)
    A23.gas_subsector_shrwt_interpto <- get_data(all_data, "gcam-usa/A23.gas_subsector_shrwt_interpto", strip_attributes = TRUE)
    A23.gas_tech_associations <- get_data(all_data, "gcam-usa/A23.gas_tech_associations", strip_attributes = TRUE)

    L111.Prod_EJ_R_F_Yh_USA <- get_data(all_data, "L111.Prod_EJ_R_F_Yh_USA", strip_attributes = TRUE)
    L222.StubTechProd_gasproc <- get_data(all_data, "L222.StubTechProd_gasproc", strip_attributes = TRUE)
    L239.PrimaryConsKeyword_en <- get_data(all_data, "L239.PrimaryConsKeyword_en", strip_attributes = TRUE)

    # ===================================================
    # Perform computations

    # Delete USA natural gas subresource
    L2262.DeleteRsrc_NG_USA <- tibble(region = "USA", resource = "natural gas")

    # Delete "regional natural gas" subsector
    A23.gas_sector_vertical %>%
      filter(supplysector == "regional natural gas") %>%
      mutate(region = "USA") %>%
      select(region, supplysector) -> L2262.Delete_regNG_supplysector

    # Modify regional natural gas sector to aggregate domestic and imported resources
    # logit for USA regional natural gas
    A23.gas_sector_vertical %>%
      filter(supplysector == "regional natural gas") %>%
      mutate(region = "USA") %>%
      mutate(logit.year.fillout = MODEL_BASE_YEARS[1]) %>%
      mutate(logit.type=NA) %>%
      select(region, supplysector, output.unit, input.unit, price.unit,
             logit.year.fillout, logit.exponent, logit.type) ->
      L2262.Supplysector_regNG_USA

    # logit for USA domestic natural gas and imported natural gas
    # Note these logit assumptions do not matter as there is no competition at this nest
    A23.gas_tech_associations %>%
      mutate(region = "USA") %>%
      left_join(A23.gas_sector_vertical, by = c("supplysector")) %>%
      mutate(logit.year.fillout = MODEL_BASE_YEARS[1]) %>%
      mutate(logit.type=NA) %>%
      select(region, supplysector, subsector, logit.year.fillout, logit.exponent, logit.type) ->
      L2262.Subsector_regNG_USA

    # Shareweights for USA domestic natural gas and imported natural gas
    L2262.SubsectorShrwt_regNG_USA <- set_years(A23.gas_subsector_shrwt)
    L2262.SubsectorShrwtInterp_regNG_USA <- set_years(A23.gas_subsector_shrwt_interp)
    L2262.SubsectorShrwtInterpTo_regNG_USA <- set_years(A23.gas_subsector_shrwt_interpto)

    # Provide calOutputValue for "regional natural gas" market
    L111.Prod_EJ_R_F_Yh_USA %>%
      mutate(region = "USA") %>%
      group_by(region, year) %>%
      summarise(calOutputValue = sum(value)) %>%
      ungroup() %>%
      left_join_error_no_match(A23.gas_tech_associations %>% filter(subsector == "domestic natural gas") %>%
                                 mutate(region = "USA"), by = c("region")) %>%
      select(region, supplysector, subsector, stub.technology, year, calOutputValue) -> L2262.CalOutput_domNG

    L222.StubTechProd_gasproc %>%
      filter(region == "USA", subsector == "natural gas") %>%
      rename(share.weight.year = year.share.weight) %>%
      left_join_keep_first_only(L2262.CalOutput_domNG %>%
                                  rename(domNG = calOutputValue) %>%
                                  select(region, year, domNG),
                                by = c("region", "year")) %>%
      mutate(calOutputValue = calOutputValue - domNG) %>%
      select(-domNG) -> L2262.Demand_impNG

    A23.gas_tech_associations %>%
      select(supplysector, subsector, stub.technology) %>%
      filter(subsector == "imported natural gas") %>%
      repeat_add_columns(tibble("year" = MODEL_BASE_YEARS)) %>%
      left_join_error_no_match(L2262.Demand_impNG %>% select(region, year, calOutputValue), by = c("year")) ->
      L2262.CalOutput_impNG

    L2262.CalOutput_domNG %>%
      bind_rows(L2262.CalOutput_impNG) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(share.weight.year = year) %>%
      mutate(subs.share.weight = 1) %>%
      mutate(tech.share.weight = 1) %>%
      mutate(calOutputValue = round(calOutputValue, 7)) -> L2262.StubTechProd_regNG_USA

    # Global technology databases for USA domestic natural gas and imported natural gas
    A23.gas_tech_associations %>%
      repeat_add_columns(tibble("year" = MODEL_BASE_YEARS)) %>%
      rename(technology = stub.technology) %>%
      mutate(region = "USA",
             market.name = "USA") %>%
      select(LEVEL2_DATA_NAMES[["TechCoef"]]) -> L2262.TechCoef_regNG_USA

    A23.gas_tech_associations %>%
      repeat_add_columns(tibble("year" = MODEL_BASE_YEARS)) %>%
      select(supplysector, subsector, technology = stub.technology, year) %>%
      mutate(region = "USA") %>%
      left_join_error_no_match(L239.PrimaryConsKeyword_en %>%
                                 select(supplysector, year, primary.consumption) %>% distinct(),
                               by = c("supplysector", "year")) %>%
      select(LEVEL2_DATA_NAMES[["PrimaryConsKeywordff"]]) ->
      L2262.PrimaryConsKeyword_regNG_USA

    # Share-weights for USA regional natural gas technologies
    A23.gas_tech_associations %>%
      repeat_add_columns(tibble("year" = MODEL_BASE_YEARS)) %>%
      rename(technology = stub.technology) %>%
      mutate(share.weight = 1) %>%
      mutate(region = "USA") %>%
      select(LEVEL2_DATA_NAMES[["TechShrwt"]]) -> L2262.TechShrwt_regNG_USA


    # ===================================================
    # Produce outputs

    L2262.DeleteRsrc_NG_USA %>%
      add_title("Delete USA natural gas subresource") %>%
      add_units("unitless") %>%
      add_comments("Delete USA natural gas subresource") ->
      L2262.DeleteRsrc_NG_USA

    L2262.Delete_regNG_supplysector %>%
      add_title("Delete regional natural gas supplysector") %>%
      add_units("unitless") %>%
      add_comments("Delete regional natural gas supplysector") %>%
      add_precursors("gcam-usa/A23.gas_sector_vertical") ->
      L2262.Delete_regNG_supplysector

    L2262.Supplysector_regNG_USA %>%
      add_title("logit for USA regional natural gas") %>%
      add_units("NA") %>%
      add_comments("logit for USA regional natural gas") %>%
      add_precursors("gcam-usa/A23.gas_sector_vertical") ->
      L2262.Supplysector_regNG_USA

    L2262.Subsector_regNG_USA %>%
      add_title("logit for USA domestic natural gas and imported natural gas") %>%
      add_units("NA") %>%
      add_comments("logit for USA domestic natural gas and imported natural gas") %>%
      add_precursors("gcam-usa/A23.gas_tech_associations",
                     "gcam-usa/A23.gas_sector_vertical") ->
      L2262.Subsector_regNG_USA

    L2262.PrimaryConsKeyword_regNG_USA %>%
      add_title("Global technology databases for USA domestic natural gas and imported natural gas") %>%
      add_units("unitless") %>%
      add_comments("Global technology databases for USA domestic natural gas and imported natural gas") %>%
      add_precursors("gcam-usa/A23.gas_tech_associations",
                     "L239.PrimaryConsKeyword_en") ->
      L2262.PrimaryConsKeyword_regNG_USA

    L2262.StubTechProd_regNG_USA %>%
      add_title("Provide calOutputValue for regional natural gas market") %>%
      add_units("EJ") %>%
      add_comments("Provide calOutputValue for regional natural gas market") %>%
      add_precursors("gcam-usa/A23.gas_tech_associations",
                     "L111.Prod_EJ_R_F_Yh_USA",
                     "L222.StubTechProd_gasproc") ->
      L2262.StubTechProd_regNG_USA

    L2262.TechCoef_regNG_USA %>%
      add_title("Global technology databases for USA domestic natural gas and imported natural gas") %>%
      add_units("unitless") %>%
      add_comments("Global technology databases for USA domestic natural gas and imported natural gas") %>%
      add_precursors("gcam-usa/A23.gas_tech_associations") ->
      L2262.TechCoef_regNG_USA

    L2262.TechShrwt_regNG_USA %>%
      add_title("Share-weights for USA regional natural gas technologies") %>%
      add_units("unitless") %>%
      add_comments("Share-weights for USA regional natural gas technologies") %>%
      add_precursors("gcam-usa/A23.gas_tech_associations") ->
      L2262.TechShrwt_regNG_USA

    L2262.SubsectorShrwt_regNG_USA %>%
      add_title("Shareweights for USA domestic natural gas and imported natural gas") %>%
      add_units("unitless") %>%
      add_comments("Shareweights for USA domestic natural gas and imported natural gas") %>%
      add_precursors("gcam-usa/A23.gas_subsector_shrwt") ->
      L2262.SubsectorShrwt_regNG_USA

    L2262.SubsectorShrwtInterp_regNG_USA %>%
      add_title("Shareweights for USA domestic natural gas and imported natural gas") %>%
      add_units("unitless") %>%
      add_comments("Shareweights for USA domestic natural gas and imported natural gas") %>%
      add_precursors("gcam-usa/A23.gas_subsector_shrwt_interp") ->
      L2262.SubsectorShrwtInterp_regNG_USA

    L2262.SubsectorShrwtInterpTo_regNG_USA %>%
      add_title("Shareweights for USA domestic natural gas and imported natural gas") %>%
      add_units("unitless") %>%
      add_comments("Shareweights for USA domestic natural gas and imported natural gas") %>%
      add_precursors("gcam-usa/A23.gas_subsector_shrwt_interpto") ->
      L2262.SubsectorShrwtInterpTo_regNG_USA

    return_data(L2262.DeleteRsrc_NG_USA,
                L2262.Delete_regNG_supplysector,
                L2262.Supplysector_regNG_USA,
                L2262.Subsector_regNG_USA,
                L2262.PrimaryConsKeyword_regNG_USA,
                L2262.StubTechProd_regNG_USA,
                L2262.TechCoef_regNG_USA,
                L2262.TechShrwt_regNG_USA,
                L2262.SubsectorShrwt_regNG_USA,
                L2262.SubsectorShrwtInterp_regNG_USA,
                L2262.SubsectorShrwtInterpTo_regNG_USA)
  } else {
    stop("Unknown command")
  }
}
