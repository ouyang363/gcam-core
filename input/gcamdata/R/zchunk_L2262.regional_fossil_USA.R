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
    return(c(FILE = "gcam-usa/A10.fossil_sector_vertical",
             FILE = "gcam-usa/A10.fossil_subsector_shrwt",
             FILE = "gcam-usa/A10.fossil_subsector_shrwt_interp",
             FILE = 'gcam-usa/A10.fossil_subsector_shrwt_interpto',
             FILE = "gcam-usa/A10.fossil_tech_associations",
             "L111.Prod_EJ_R_F_Yh_USA",
             "L222.StubTechProd_gasproc",
             "L222.StubTechProd_refining",
             "L222.StubTechCoef_refining",
             "L239.PrimaryConsKeyword_en"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2262.DeleteRsrc_fos_USA",
             "L2262.Delete_reg_fos_supplysector",
             "L2262.Supplysector_reg_fos_USA",
             "L2262.Subsector_reg_fos_USA",
             "L2262.PrimaryConsKeyword_reg_fos_USA",
             "L2262.StubTechProd_reg_fos_USA",
             "L2262.TechCoef_reg_fos_USA",
             "L2262.TechShrwt_reg_fos_USA",
             "L2262.SubsectorShrwt_reg_fos_USA",
             "L2262.SubsectorShrwtInterp_reg_fos_USA",
             "L2262.SubsectorShrwtInterpTo_reg_fos_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # silence check package notes
    # TODO to update
    from.year <- input.cost <- input.unit <- logit.exponent <- logit.type <- market.name <-
      minicam.non.energy.input <- output.unit <- price.unit <- region <- sector.name <- state <-
      stub.technology <- subsector <- supplysector <- technology <- to.year <- year <- traded <-
      subsector.name <- share.weight <- fractional.secondary.output <- price <- fraction.produced <-
      PrimaryFuelCO2Coef.name <- PrimaryFuelCO2Coef <- minicam.energy.input <- sector <- calibrated.value <-
      value <- share <- fuel <- . <- output.ratio <- subs.share.weight <- tech.share.weight <-
      subsector_1 <- to.technology <- NULL

    # Load required inputs
    A10.fossil_sector_vertical <- get_data(all_data, "gcam-usa/A10.fossil_sector_vertical", strip_attributes = TRUE)
    A10.fossil_subsector_shrwt <- get_data(all_data, "gcam-usa/A10.fossil_subsector_shrwt", strip_attributes = TRUE)
    A10.fossil_subsector_shrwt_interp <- get_data(all_data, "gcam-usa/A10.fossil_subsector_shrwt_interp", strip_attributes = TRUE)
    A10.fossil_subsector_shrwt_interpto <- get_data(all_data, "gcam-usa/A10.fossil_subsector_shrwt_interpto", strip_attributes = TRUE)
    A10.fossil_tech_associations <- get_data(all_data, "gcam-usa/A10.fossil_tech_associations", strip_attributes = TRUE)

    L111.Prod_EJ_R_F_Yh_USA <- get_data(all_data, "L111.Prod_EJ_R_F_Yh_USA", strip_attributes = TRUE)
    L222.StubTechProd_gasproc <- get_data(all_data, "L222.StubTechProd_gasproc", strip_attributes = TRUE)
    L222.StubTechProd_refining <- get_data(all_data, "L222.StubTechProd_refining", strip_attributes = TRUE)
    L222.StubTechCoef_refining <- get_data(all_data, "L222.StubTechCoef_refining", strip_attributes = TRUE)
    L239.PrimaryConsKeyword_en <- get_data(all_data, "L239.PrimaryConsKeyword_en", strip_attributes = TRUE)

    # ===================================================
    # Perform computations

    # Delete USA fossil subresource
    L2262.DeleteRsrc_fos_USA <- tibble(region = "USA", resource = c("natural gas", "crude oil", "coal"))

    # Delete "regional [fossil resource]" subsector
    A10.fossil_sector_vertical %>%
      filter(grepl("regional", supplysector)) %>%
      mutate(region = "USA") %>%
      select(region, supplysector) -> L2262.Delete_reg_fos_supplysector

    # Modify regional [fossil resource] sector to aggregate domestic and imported resources
    # logit for USA regional [fossil resource]
    A10.fossil_sector_vertical %>%
      filter(grepl("regional", supplysector)) %>%
      mutate(region = "USA") %>%
      mutate(logit.year.fillout = MODEL_BASE_YEARS[1]) %>%
      mutate(logit.type=NA) %>%
      select(region, supplysector, output.unit, input.unit, price.unit,
             logit.year.fillout, logit.exponent, logit.type) ->
      L2262.Supplysector_reg_fos_USA

    # logit for USA domestic [fossil resource] and imported [fossil resource]
    # Note these logit assumptions do not matter as there is no competition at this nest
    A10.fossil_tech_associations %>%
      mutate(region = "USA") %>%
      left_join(A10.fossil_sector_vertical, by = c("supplysector")) %>%
      mutate(logit.year.fillout = MODEL_BASE_YEARS[1]) %>%
      mutate(logit.type=NA) %>%
      select(region, supplysector, subsector, logit.year.fillout, logit.exponent, logit.type) ->
      L2262.Subsector_reg_fos_USA

    # Shareweights for USA domestic [fossil resource] and imported [fossil resource]
    L2262.SubsectorShrwt_reg_fos_USA <- set_years(A10.fossil_subsector_shrwt)
    L2262.SubsectorShrwtInterp_reg_fos_USA <- set_years(A10.fossil_subsector_shrwt_interp)
    L2262.SubsectorShrwtInterpTo_reg_fos_USA <- set_years(A10.fossil_subsector_shrwt_interpto)

    # Provide calOutputValue for "regional [fossil resource]" market
    L111.Prod_EJ_R_F_Yh_USA %>%
      mutate(region = "USA") %>%
      group_by(region, resource, year) %>%
      summarise(calOutputValue = sum(value)) %>%
      ungroup() %>%
      # create a temp variable for mapping
      mutate(subsector = case_when(
        resource == "crude oil" ~ "domestic oil",
        resource == "natural gas" ~ "domestic natural gas",
        resource == "coal" ~ "domestic coal"
      )) %>%
      left_join_error_no_match(A10.fossil_tech_associations %>% filter(grepl("domestic", subsector)) %>%
                                 mutate(region = "USA"), by = c("region", "subsector")) %>%
      select(region, supplysector, subsector, stub.technology, year, calOutputValue) -> L2262.CalOutput_dom_fos

    # update gas processing sector
    L222.StubTechProd_gasproc %>%
      filter(region == "USA", subsector == "natural gas") %>%
      rename(share.weight.year = year.share.weight) %>%
      left_join_keep_first_only(L2262.CalOutput_dom_fos %>%
                                  filter(stub.technology == "domestic natural gas") %>%
                                  rename(domNG = calOutputValue) %>%
                                  select(region, year, domNG),
                                by = c("region", "year")) %>%
      mutate(calOutputValue = calOutputValue - domNG) %>%
      select(-domNG) -> L2262.Demand_impNG

    A10.fossil_tech_associations %>%
      select(supplysector, subsector, stub.technology) %>%
      filter(subsector == "imported natural gas") %>%
      repeat_add_columns(tibble("year" = MODEL_BASE_YEARS)) %>%
      left_join_error_no_match(L2262.Demand_impNG %>% select(region, year, calOutputValue), by = c("year")) ->
      L2262.CalOutput_impNG

    # update oil processing sector
    L222.StubTechProd_refining %>%
      filter(region == "USA", subsector == "oil refining") %>%
      rename(share.weight.year = year.share.weight) %>%
      # for oil refinery, there is a conversion loss in the refinery sector, so backward calculate oil input to the refinery sector
      # input = output * coefficient
      left_join_error_no_match(L222.StubTechCoef_refining %>%
                                 filter(region == "USA" & minicam.energy.input == "regional oil") %>%
                                 select(year, coefficient), by = "year") %>%
      mutate(calOutputValue = calOutputValue * coefficient) %>%
      # then subtract domestic production to obain imported values
      left_join_keep_first_only(L2262.CalOutput_dom_fos %>%
                                  filter(stub.technology == "domestic oil") %>%
                                  rename(domOL = calOutputValue) %>%
                                  select(region, year, domOL),
                                by = c("region", "year")) %>%
      mutate(calOutputValue = calOutputValue - domOL) %>%
      select(-domOL) -> L2262.Demand_impOL

    A10.fossil_tech_associations %>%
      select(supplysector, subsector, stub.technology) %>%
      filter(subsector == "imported oil") %>%
      repeat_add_columns(tibble("year" = MODEL_BASE_YEARS)) %>%
      left_join_error_no_match(L2262.Demand_impOL %>% select(region, year, calOutputValue), by = c("year")) ->
      L2262.CalOutput_impOL

    # combine imported with domestic production
    L2262.CalOutput_dom_fos %>%
      bind_rows(L2262.CalOutput_impNG) %>%
      bind_rows(L2262.CalOutput_impOL) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(share.weight.year = year) %>%
      mutate(subs.share.weight = 1) %>%
      mutate(tech.share.weight = 1) %>%
      mutate(calOutputValue = round(calOutputValue, 7)) -> L2262.StubTechProd_reg_fos_USA

    # Global technology databases for USA domestic [fossil resource] and imported [fossil resource]
    A10.fossil_tech_associations %>%
      repeat_add_columns(tibble("year" = MODEL_BASE_YEARS)) %>%
      rename(technology = stub.technology) %>%
      mutate(region = "USA",
             market.name = "USA") %>%
      select(LEVEL2_DATA_NAMES[["TechCoef"]]) -> L2262.TechCoef_reg_fos_USA

    A10.fossil_tech_associations %>%
      repeat_add_columns(tibble("year" = MODEL_BASE_YEARS)) %>%
      select(supplysector, subsector, technology = stub.technology, year) %>%
      mutate(region = "USA") %>%
      left_join_error_no_match(L239.PrimaryConsKeyword_en %>%
                                 select(supplysector, year, primary.consumption) %>% distinct(),
                               by = c("supplysector", "year")) %>%
      select(LEVEL2_DATA_NAMES[["PrimaryConsKeywordff"]]) ->
      L2262.PrimaryConsKeyword_reg_fos_USA

    # Share-weights for USA regional natural gas technologies
    A10.fossil_tech_associations %>%
      repeat_add_columns(tibble("year" = MODEL_BASE_YEARS)) %>%
      rename(technology = stub.technology) %>%
      mutate(share.weight = 1) %>%
      mutate(region = "USA") %>%
      select(LEVEL2_DATA_NAMES[["TechShrwt"]]) -> L2262.TechShrwt_reg_fos_USA


    # ===================================================
    # Produce outputs

    L2262.DeleteRsrc_fos_USA %>%
      add_title("Delete USA natural gas subresource") %>%
      add_units("unitless") %>%
      add_comments("Delete USA natural gas subresource") ->
      L2262.DeleteRsrc_fos_USA

    L2262.Delete_reg_fos_supplysector %>%
      add_title("Delete regional natural gas supplysector") %>%
      add_units("unitless") %>%
      add_comments("Delete regional natural gas supplysector") %>%
      add_precursors("gcam-usa/A10.fossil_sector_vertical") ->
      L2262.Delete_reg_fos_supplysector

    L2262.Supplysector_reg_fos_USA %>%
      add_title("logit for USA regional natural gas") %>%
      add_units("NA") %>%
      add_comments("logit for USA regional natural gas") %>%
      add_precursors("gcam-usa/A10.fossil_sector_vertical") ->
      L2262.Supplysector_reg_fos_USA

    L2262.Subsector_reg_fos_USA %>%
      add_title("logit for USA domestic natural gas and imported natural gas") %>%
      add_units("NA") %>%
      add_comments("logit for USA domestic natural gas and imported natural gas") %>%
      add_precursors("gcam-usa/A10.fossil_tech_associations",
                     "gcam-usa/A10.fossil_sector_vertical") ->
      L2262.Subsector_reg_fos_USA

    L2262.PrimaryConsKeyword_reg_fos_USA %>%
      add_title("Global technology databases for USA domestic natural gas and imported natural gas") %>%
      add_units("unitless") %>%
      add_comments("Global technology databases for USA domestic natural gas and imported natural gas") %>%
      add_precursors("gcam-usa/A10.fossil_tech_associations",
                     "L239.PrimaryConsKeyword_en") ->
      L2262.PrimaryConsKeyword_reg_fos_USA

    L2262.StubTechProd_reg_fos_USA %>%
      add_title("Provide calOutputValue for regional natural gas market") %>%
      add_units("EJ") %>%
      add_comments("Provide calOutputValue for regional natural gas market") %>%
      add_precursors("gcam-usa/A10.fossil_tech_associations",
                     "L111.Prod_EJ_R_F_Yh_USA",
                     "L222.StubTechProd_gasproc",
                     "L222.StubTechProd_refining",
                     "L222.StubTechCoef_refining") ->
      L2262.StubTechProd_reg_fos_USA

    L2262.TechCoef_reg_fos_USA %>%
      add_title("Global technology databases for USA domestic natural gas and imported natural gas") %>%
      add_units("unitless") %>%
      add_comments("Global technology databases for USA domestic natural gas and imported natural gas") %>%
      add_precursors("gcam-usa/A10.fossil_tech_associations") ->
      L2262.TechCoef_reg_fos_USA

    L2262.TechShrwt_reg_fos_USA %>%
      add_title("Share-weights for USA regional natural gas technologies") %>%
      add_units("unitless") %>%
      add_comments("Share-weights for USA regional natural gas technologies") %>%
      add_precursors("gcam-usa/A10.fossil_tech_associations") ->
      L2262.TechShrwt_reg_fos_USA

    L2262.SubsectorShrwt_reg_fos_USA %>%
      add_title("Shareweights for USA domestic natural gas and imported natural gas") %>%
      add_units("unitless") %>%
      add_comments("Shareweights for USA domestic natural gas and imported natural gas") %>%
      add_precursors("gcam-usa/A10.fossil_subsector_shrwt") ->
      L2262.SubsectorShrwt_reg_fos_USA

    L2262.SubsectorShrwtInterp_reg_fos_USA %>%
      add_title("Shareweights for USA domestic natural gas and imported natural gas") %>%
      add_units("unitless") %>%
      add_comments("Shareweights for USA domestic natural gas and imported natural gas") %>%
      add_precursors("gcam-usa/A10.fossil_subsector_shrwt_interp") ->
      L2262.SubsectorShrwtInterp_reg_fos_USA

    L2262.SubsectorShrwtInterpTo_reg_fos_USA %>%
      add_title("Shareweights for USA domestic natural gas and imported natural gas") %>%
      add_units("unitless") %>%
      add_comments("Shareweights for USA domestic natural gas and imported natural gas") %>%
      add_precursors("gcam-usa/A10.fossil_subsector_shrwt_interpto") ->
      L2262.SubsectorShrwtInterpTo_reg_fos_USA

    return_data(L2262.DeleteRsrc_fos_USA,
                L2262.Delete_reg_fos_supplysector,
                L2262.Supplysector_reg_fos_USA,
                L2262.Subsector_reg_fos_USA,
                L2262.PrimaryConsKeyword_reg_fos_USA,
                L2262.StubTechProd_reg_fos_USA,
                L2262.TechCoef_reg_fos_USA,
                L2262.TechShrwt_reg_fos_USA,
                L2262.SubsectorShrwt_reg_fos_USA,
                L2262.SubsectorShrwtInterp_reg_fos_USA,
                L2262.SubsectorShrwtInterpTo_reg_fos_USA)
  } else {
    stop("Unknown command")
  }
}
