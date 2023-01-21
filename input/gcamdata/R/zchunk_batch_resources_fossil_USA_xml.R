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
             "L211.TNGTechCoef_USA",
             "L2262.DeleteRsrc_NG_USA",
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
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "resources_fossil_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L211.PrimaryCO2Coef_USA <- get_data(all_data, "L211.PrimaryCO2Coef_USA")
    L211.Rsrc_F_USA <- get_data(all_data, "L211.Rsrc_F_USA")
    L211.RsrcPrice_F_USA <- get_data(all_data, "L211.RsrcPrice_F_USA")
    L211.RsrcCalProd_USA <- get_data(all_data, "L211.RsrcCalProd_USA")
    L211.RsrcTechChange_USA <- get_data(all_data, "L211.RsrcTechChange_USA")
    L211.RsrcCurves_fos_USA <- get_data(all_data, "L211.RsrcCurves_fos_USA")

    L211.ResSubresourceProdLifetime_USA <- get_data(all_data, "L211.ResSubresourceProdLifetime_USA")
    L211.SubresourcePriceAdder_USA <- get_data(all_data, "L211.SubresourcePriceAdder_USA")
    L211.ReserveCalReserve_USA <- get_data(all_data, "L211.ReserveCalReserve_USA")
    L211.ResReserveTechLifetime_USA <- get_data(all_data, "L211.ResReserveTechLifetime_USA")
    L211.ResReserveTechDeclinePhase_USA <- get_data(all_data, "L211.ResReserveTechDeclinePhase_USA")
    L211.ResReserveTechProfitShutdown_USA <- get_data(all_data, "L211.ResReserveTechProfitShutdown_USA")

    L211.Sector_prod_USA <- get_data(all_data, "L211.Sector_prod_USA")
    L211.Subsector_prod_USA <- get_data(all_data, "L211.Subsector_prod_USA")
    L211.SubsShrwtFlt_USA <- get_data(all_data, "L211.SubsShrwtFlt_USA")
    L211.SubsInterpRule_USA <- get_data(all_data, "L211.SubsInterpRule_USA")
    L211.TechShrwt_USA <- get_data(all_data, "L211.TechShrwt_USA")
    L211.TechCoef_USA <- get_data(all_data, "L211.TechCoef_USA")
    L211.TechCal_USA <- get_data(all_data, "L211.TechCal_USA")
    L211.TNGSubsectorLogit <- get_data(all_data, "L211.TNGSubsectorLogit")
    L211.TNGTechProduction_USA <- get_data(all_data, "L211.TNGTechProduction_USA")
    L211.TNGTechCoef_USA <- get_data(all_data, "L211.TNGTechCoef_USA")

    L2262.DeleteRsrc_NG_USA <- get_data(all_data, "L2262.DeleteRsrc_NG_USA")
    L2262.Delete_regNG_supplysector <- get_data(all_data, "L2262.Delete_regNG_supplysector")
    L2262.Supplysector_regNG_USA <- get_data(all_data, "L2262.Supplysector_regNG_USA")
    L2262.Subsector_regNG_USA <- get_data(all_data, "L2262.Subsector_regNG_USA")
    L2262.PrimaryConsKeyword_regNG_USA <- get_data(all_data, "L2262.PrimaryConsKeyword_regNG_USA")
    L2262.StubTechProd_regNG_USA <- get_data(all_data, "L2262.StubTechProd_regNG_USA")
    L2262.TechCoef_regNG_USA <- get_data(all_data, "L2262.TechCoef_regNG_USA")
    L2262.TechShrwt_regNG_USA <- get_data(all_data, "L2262.TechShrwt_regNG_USA")
    L2262.SubsectorShrwt_regNG_USA <- get_data(all_data, "L2262.SubsectorShrwt_regNG_USA")
    L2262.SubsectorShrwtInterp_regNG_USA <- get_data(all_data, "L2262.SubsectorShrwtInterp_regNG_USA")
    L2262.SubsectorShrwtInterpTo_regNG_USA <- get_data(all_data, "L2262.SubsectorShrwtInterpTo_regNG_USA")


    # ===================================================

    L211.RsrcCurves_fos_USA <- L211.RsrcCurves_fos_USA %>%
      rename(subresource = reserve.subresource)

    L211.RsrcCalProd_USA <- L211.RsrcCalProd_USA %>%
      rename(subresource = reserve.subresource)

    L211.RsrcTechChange_USA <- L211.RsrcTechChange_USA %>%
      rename(subresource = reserve.subresource)

    L211.SubresourcePriceAdder_USA <- L211.SubresourcePriceAdder_USA %>%
      rename(subresource = reserve.subresource)


    # Produce outputs
    create_xml("resources_fossil_USA.xml") %>%
      add_xml_data(L2262.DeleteRsrc_NG_USA, "DeleteRsrc") %>%
      add_xml_data(L2262.Delete_regNG_supplysector, "DeleteSupplysector") %>%
      # resource curve
      add_xml_data(L211.PrimaryCO2Coef_USA, "CarbonCoef") %>%
      add_xml_data(L211.Rsrc_F_USA, "Rsrc") %>%
      add_node_equiv_xml("resource") %>%
      add_node_equiv_xml("subresource") %>%
      add_node_equiv_xml("technology") %>%
      # add resource.reserve.technology details
      add_xml_data(L211.ResSubresourceProdLifetime_USA, "ResSubresourceProdLifetime") %>%
      add_xml_data(L211.SubresourcePriceAdder_USA, "SubresourcePriceAdder") %>%
      add_xml_data(L211.ReserveCalReserve_USA, "ReserveCalReserve") %>%
      add_xml_data(L211.ResReserveTechLifetime_USA, "ResReserveTechLifetime") %>%
      add_xml_data(L211.ResReserveTechDeclinePhase_USA, "ResReserveTechDeclinePhase") %>%
      add_xml_data(L211.ResReserveTechProfitShutdown_USA, "ResReserveTechProfitShutdown") %>%
      add_xml_data(L211.RsrcPrice_F_USA, "RsrcPrice") %>%
      add_xml_data(L211.RsrcTechChange_USA, "RsrcTechChange") %>%
      add_xml_data(L211.RsrcCalProd_USA, "RsrcCalProd") %>%
      add_xml_data(L211.RsrcCurves_fos_USA, "RsrcCurves") %>%
      # natural gas production sector
      add_logit_tables_xml(L211.Sector_prod_USA, "Supplysector") %>%
      add_logit_tables_xml(L211.Subsector_prod_USA, "SubsectorLogit") %>%
      add_xml_data(L211.SubsShrwtFlt_USA, "SubsectorShrwtFllt") %>%
      add_xml_data(L211.SubsInterpRule_USA, "SubsectorInterp") %>%
      add_xml_data(L211.TechShrwt_USA, "TechShrwt") %>%
      add_xml_data(L211.TechCoef_USA, "TechCoef") %>%
      add_xml_data(L211.TechCal_USA, "StubTechCalInput") %>%
      add_logit_tables_xml(L211.TNGSubsectorLogit, "SubsectorLogit") %>%
      add_xml_data(L211.TNGTechProduction_USA, "Production") %>%
      add_xml_data(L211.TNGTechCoef_USA, "TechCoef") %>%
      # domestic versus import NG
      add_logit_tables_xml(L2262.Supplysector_regNG_USA, "Supplysector") %>%
      add_logit_tables_xml(L2262.Subsector_regNG_USA, "SubsectorLogit") %>%
      add_xml_data(L2262.PrimaryConsKeyword_regNG_USA, "PrimaryConsKeywordff") %>%
      add_xml_data(L2262.StubTechProd_regNG_USA, "StubTechProd") %>%
      add_xml_data(L2262.TechCoef_regNG_USA, "TechCoef") %>%
      add_xml_data(L2262.TechShrwt_regNG_USA, "TechShrwt") %>%
      add_xml_data(L2262.SubsectorShrwt_regNG_USA, "SubsectorShrwt") %>%
      add_xml_data(L2262.SubsectorShrwtInterp_regNG_USA, "SubsectorInterp") %>%
      add_xml_data(L2262.SubsectorShrwtInterpTo_regNG_USA, "SubsectorInterpTo") %>%
      add_precursors("L211.PrimaryCO2Coef_USA",
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
                     "L211.TNGTechCoef_USA",
                     "L2262.DeleteRsrc_NG_USA",
                     "L2262.Delete_regNG_supplysector",
                     "L2262.Supplysector_regNG_USA",
                     "L2262.Subsector_regNG_USA",
                     "L2262.PrimaryConsKeyword_regNG_USA",
                     "L2262.StubTechProd_regNG_USA",
                     "L2262.TechCoef_regNG_USA",
                     "L2262.TechShrwt_regNG_USA",
                     "L2262.SubsectorShrwt_regNG_USA",
                     "L2262.SubsectorShrwtInterp_regNG_USA",
                     "L2262.SubsectorShrwtInterpTo_regNG_USA") ->
      resources_fossil_USA.xml

    return_data(resources_fossil_USA.xml)
  } else {
    stop("Unknown command")
  }
}
