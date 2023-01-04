# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_batch_resources_fossil_USA_xml
#'
#' Construct XML data structure for \code{resources_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{resources_fossil_USA.xml}. The corresponding file in the
#' original data system was \code{batch_resources_USA_xml.R} (gcamusa XML).
module_gcamusa_batch_resources_fossil_USA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L211.PrimaryCO2Coef",
             "L211.Depresource",
             "L211.DepresourcePrice",
             "L211.DepresourceCal",
             "L211.DepresourceTechChange",
             "L211.Grades",
             "L211.Sector",
             "L211.Subsector",
             "L211.SubsShrwtFlt",
             "L211.SubsInterpRule",
             "L211.Subs_shrwt",
             "L211.GlobalDBTechShrwt",
             "L211.GlobalDBTechCoef",
             "L211.TechCal",
             "L211.TechStubs",
             "L211.TNGTechProduction",
             "L211.TNGTechCoef",
             "L211.InterestRate_Offshore",
             "L211.Pop_Offshore",
             "L211.BaseGDP_Offshore",
             "L211.LaborForceFillout_Offshore"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "resources_fossil_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L211.PrimaryCO2Coef <- get_data(all_data, "L211.PrimaryCO2Coef")
    L211.Depresource <- get_data(all_data, "L211.Depresource")
    L211.DepresourcePrice <- get_data(all_data, "L211.DepresourcePrice")
    L211.DepresourceCal <- get_data(all_data, "L211.DepresourceCal")
    L211.DepresourceTechChange <- get_data(all_data, "L211.DepresourceTechChange")
    L211.Grades <- get_data(all_data, "L211.Grades")
    L211.Sector <- get_data(all_data, "L211.Sector")
    L211.Subsector <- get_data(all_data, "L211.Subsector")
    L211.SubsShrwtFlt <- get_data(all_data, "L211.SubsShrwtFlt")
    L211.SubsInterpRule <- get_data(all_data, "L211.SubsInterpRule")
    L211.Subs_shrwt <- get_data(all_data, "L211.Subs_shrwt")
    L211.GlobalDBTechShrwt <- get_data(all_data, "L211.GlobalDBTechShrwt")
    L211.GlobalDBTechCoef <- get_data(all_data, "L211.GlobalDBTechCoef")
    L211.TechCal <- get_data(all_data, "L211.TechCal")
    L211.TechStubs <- get_data(all_data, "L211.TechStubs")
    L211.TNGTechProduction <- get_data(all_data, "L211.TNGTechProduction")
    L211.TNGTechCoef <- get_data(all_data, "L211.TNGTechCoef")
    L211.InterestRate_Offshore <- get_data(all_data, "L211.InterestRate_Offshore")
    L211.Pop_Offshore <- get_data(all_data, "L211.Pop_Offshore")
    L211.BaseGDP_Offshore <- get_data(all_data, "L211.BaseGDP_Offshore")
    L211.LaborForceFillout_Offshore <- get_data(all_data, "L211.LaborForceFillout_Offshore")


    # ===================================================

    L211.TechStubs <- L211.TechStubs %>% rename(stub.technology = technology)
    L211.GlobalDBTechCoef <- L211.GlobalDBTechCoef %>%
      rename(sector.name = supplysector,
             subsector.name = subsector)

    L211.GlobalDBTechShrwt <- L211.GlobalDBTechShrwt %>%
      rename(sector.name = supplysector,
             subsector.name = subsector)

    L211.SubsShrwtFlt <- L211.SubsShrwtFlt %>% rename(year.fillout = year)

    L211.Grades <- L211.Grades %>%
      rename(region = state,
             resource = depresource,
             extractioncost = cost)

    L211.DepresourceTechChange <- L211.DepresourceTechChange %>%
      rename(region = state,
             year.fillout = year,
             techChange = value)

    L211.DepresourceCal <- L211.DepresourceCal %>%
      rename(resource = depresource)

    L211.DepresourcePrice <- L211.DepresourcePrice %>%
      rename(region = state)

    L211.Depresource <- L211.Depresource %>%
      rename(region = state)


    # Produce outputs
    create_xml("resources_fossil_USA.xml") %>%
      add_xml_data(L211.PrimaryCO2Coef, "CarbonCoef") %>%
      add_xml_data(L211.Depresource, "Rsrc") %>%
      add_xml_data(L211.DepresourcePrice, "RsrcPrice") %>%
      add_xml_data(L211.DepresourceCal, "RsrcCalProd") %>%
      add_xml_data(L211.DepresourceTechChange, "RsrcTechChange") %>%
      add_xml_data(L211.Grades, "RsrcCurves") %>%
      add_xml_data(L211.SubsShrwtFlt, "SubsectorShrwtFllt") %>%
      add_xml_data(L211.Subs_shrwt, "SubsectorShrwt") %>%
      add_node_equiv_xml("resource") %>%
      add_node_equiv_xml("subresource") %>%
      add_node_equiv_xml("technology") %>%
      add_xml_data(L211.GlobalDBTechShrwt, "GlobalTechShrwt") %>%
      add_xml_data(L211.GlobalDBTechCoef, "GlobalTechCoef") %>%
      add_xml_data(L211.TechCal, "StubTechCalInput") %>%
      add_xml_data(L211.TechStubs, "StubTech") %>%
      add_xml_data(L211.TNGTechProduction, "Production") %>%
      add_xml_data(L211.TNGTechCoef, "TechCoef") %>%
      add_xml_data(L211.InterestRate_Offshore, "InterestRate") %>%
      add_xml_data(L211.Pop_Offshore, "Pop") %>%
      add_xml_data(L211.BaseGDP_Offshore, "BaseGDP") %>%
      add_xml_data(L211.LaborForceFillout_Offshore, "LaborForceFillout") %>%
      add_precursors("L211.PrimaryCO2Coef",
                     "L211.Depresource",
                     "L211.DepresourcePrice",
                     "L211.DepresourceCal",
                     "L211.DepresourceTechChange",
                     "L211.Grades",
                     "L211.Sector",
                     "L211.Subsector",
                     "L211.SubsShrwtFlt",
                     "L211.SubsInterpRule",
                     "L211.Subs_shrwt",
                     "L211.GlobalDBTechShrwt",
                     "L211.GlobalDBTechCoef",
                     "L211.TechCal",
                     "L211.TechStubs",
                     "L211.TNGTechProduction",
                     "L211.TNGTechCoef",
                     "L211.InterestRate_Offshore",
                     "L211.Pop_Offshore",
                     "L211.BaseGDP_Offshore",
                     "L211.LaborForceFillout_Offshore") ->
      resources_fossil_USA.xml

    return_data(resources_fossil_USA.xml)
  } else {
    stop("Unknown command")
  }
}
