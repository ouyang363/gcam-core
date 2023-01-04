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
             FILE = "gcam-usa/ETSAP_gas_cost_range",
             FILE = "gcam-usa/BOEM_gas_cost",
             FILE = "gcam-usa/A10.TechChange",
             FILE = "gcam-usa/A10.subsector_interp",
             FILE = "gcam-usa/A10.subsector_shrwt",
             "L111.gas_supply_state_T_EJ",
             "L111.gas_prod_state_T_Yh_EJ",
             "L111.unconv_gas_supply_state_EJ"))
  } else if(command == driver.DECLARE_OUTPUTS) {
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
             "L211.LaborForceFillout_Offshore")) #
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
    ETSAP_gas_cost_range <- get_data(all_data, "gcam-usa/ETSAP_gas_cost_range", strip_attributes = TRUE)
    BOEM_gas_cost <- get_data(all_data, "gcam-usa/BOEM_gas_cost", strip_attributes = TRUE)
    A10.TechChange <- get_data(all_data, "gcam-usa/A10.TechChange", strip_attributes = TRUE)
    A10.subsector_interp <- get_data(all_data, "gcam-usa/A10.subsector_interp", strip_attributes = TRUE)
    A10.subsector_shrwt <- get_data(all_data, "gcam-usa/A10.subsector_shrwt", strip_attributes = TRUE)
    L111.gas_supply_state_T_EJ <- get_data(all_data, "L111.gas_supply_state_T_EJ", strip_attributes = TRUE)
    L111.gas_prod_state_T_Yh_EJ <- get_data(all_data, "L111.gas_prod_state_T_Yh_EJ", strip_attributes = TRUE)
    L111.unconv_gas_supply_state_EJ <- get_data(all_data, "L111.unconv_gas_supply_state_EJ", strip_attributes = TRUE)

    # ===================================================
    # Perform computations

    # NOTE: FUNCTION STILL NEEDS TO BE RE-WRITTEN IN DPLYR
    compute_offshore_costs <- function( supply, costs ) {
      ret.costs <- data.frame()
      grades <- c( "grade.hist", "grade.1", "grade.2", "grade.3", "grade.max" )
      supply$cumul.1 <- supply$grade.1
      supply$cumul.2 <- supply$cumul.1 + supply$grade.2
      supply$cumul.3 <- supply$cumul.2 + supply$grade.3
      for( r in unique( costs$region ) ) {
        costs.region <- subset( costs, region == r )
        supply.region <- subset( supply, state == r )
        lmfit <- lm( formula = price ~ sqrt( quantity ) + quantity, data=costs.region )
        ret.costs.region <- data.frame( state=r, grade=grades, cost=0 )
        min.cost <- min( costs.region$price ) + 1.5
        ret.costs.region$cost[1] <- min.cost * 0.5
        ret.costs.region$cost[2] <- min.cost * 0.9
        ret.costs.region$cost[3] <- min.cost
        ret.costs.region$cost[4:5] <- predict( lmfit, data.frame( quantity=c( supply.region$cumul.2, supply.region$cumul.3 ) ) )
        # TODO: get better grasp of how "reserve adjustment factors" was taking into
        # account.  They claim a value of 0.4.
        ret.costs.region$cost[4] <- ret.costs.region$cost[4] * 0.6
        ret.costs.region$cost[4] <- pmax( ret.costs.region$cost[4], ret.costs.region$cost[3] * 1.1 )
        ret.costs.region$cost[5] <- ret.costs.region$cost[5] * 0.6
        ret.costs <- rbind( ret.costs, ret.costs.region )
      }
      return(ret.costs)
    }

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


    # Need to create dummy socio-economics for offshore resource regions
    L211.offshore_regions <- unique( BOEM_gas_cost$region )
    L211.InterestRate_Offshore <- tibble( region = L211.offshore_regions,
                                          interest.rate = socioeconomics.DEFAULT_INTEREST_RATE )

    # L211.Pop_Offshore: Population
    L211.InterestRate_Offshore %>%
      as_tibble() %>%
      repeat_add_columns(tibble("year" = MODEL_YEARS)) %>%
      mutate(totalPop = 1) %>%
      select(-interest.rate) -> L211.Pop_Offshore

    # L211.BaseGDP_Offshore: Base GDP in Offshore resource regions
    L211.BaseGDP_Offshore <- tibble( region = L211.offshore_regions, baseGDP = 1 )

    # L211.LaborForceFillout_Offshore: labor force in the Offshore resource regions
    L211.LaborForceFillout_Offshore <- tibble(
      region = L211.offshore_regions,
      year.fillout = min( MODEL_BASE_YEARS ),
      laborforce = socioeconomics.DEFAULT_LABORFORCE
    )

    # Create resource supply curves

    # L211.PrimaryCO2Coef: Add CO2 coefficients.
    L111.gas_supply_state_T_EJ %>%
      bind_rows(L111.unconv_gas_supply_state_EJ) %>%
      distinct(state, resource, .keep_all = FALSE) %>%
      mutate(co2.coef = 14.2) -> L211.PrimaryCO2Coef

    # L211.Depresource: resource parameters
    L211.PrimaryCO2Coef %>%
      select(state, resource) %>%
      mutate(output.unit = "EJ",
             price.unit = "1975$/GJ",
             market = state) -> L211.Depresource

    L211.PrimaryCO2Coef %>%
      select(state, resource) %>%
      mutate(year = MODEL_BASE_YEARS[1],
             price = 0.81) -> L211.DepresourcePrice

    # L211.DepresourceTechChange: subresource technological change
    L211.PrimaryCO2Coef %>%
      select(state, resource) %>%
      # use left_join because number of rows will be changed (copying to all years)
      left_join(A10.TechChange %>% gather(year, value, -resource, -subresource), by = c("resource")) %>%
      mutate(year = as.integer(year)) ->
      L211.DepresourceTechChange


    #L211.Grades: resource grades

    # First need to specifiy resource costs
    # Start with onshore cost esitmates
    # Arbitrary but unless we care about historical prices it doesn't matter
    ETSAP_gas_cost_range %>%
      mutate(grade.hist = low_cost * 0.5,
             grade.2 = ((high_cost - low_cost ) / 3) + low_cost) %>%
      # Assume that all gas will be used up at a cost 5 times higher than high_cost
      # TODO: better place for this assumptions
      mutate(grade.max = high_cost * 5) %>%
      gather(grade, cost, -type) %>%
      mutate(grade = if_else(grade == "low_cost", "grade.1", grade),
             grade = if_else(grade == "high_cost", "grade.3", grade)) %>%
      rename(depresource = type) %>%
      mutate(cost = cost * gdp_deflator(1975, 2008)) -> L211.GradeCost.onshore

    # Offshore cost estimates are more detailed however we will just simply these
    BOEM_gas_cost %>%
      mutate(quantity = quantity * CONV_MMCF_EJ * 1e6,
             price = (price * gdp_deflator(1975, 2005)) / (CONV_MMCF_EJ * 1e6) ) -> L211.GradeCost.offshore

    # NOTE:  compute_offshore_costs FUNCTION MUST BE RE-WRITTEN BEFORE THIS CAN BE RE-WRITTEN IN DPLYR
    L211.GradeCost.offshore <- compute_offshore_costs( L111.gas_supply_state_T_EJ, L211.GradeCost.offshore )

    L211.GradeCost.offshore %>%
      mutate(depresource = "offshore gas") -> L211.GradeCost.offshore

    # Duplicate costs by state and put on and off shore together
    L111.gas_supply_state_T_EJ %>%
      distinct(state) %>%
      filter(!grepl( 'OCS', state )) -> onshore_states
    L211.onshore_regions <- unique(onshore_states$state)

    L211.GradeCost.onshore %>%
      repeat_add_columns(tibble("state" = L211.onshore_regions)) %>%
      select(state, depresource, grade, cost) -> L211.GradeCost

    L111.unconv_gas_supply_state_EJ %>%
      rename(depresource = resource, cost = extractioncost) %>%
      select(state, depresource, grade, cost) -> L211.GradeCost.unconv

    L211.GradeCost %>%
      bind_rows(L211.GradeCost.offshore, L211.GradeCost.unconv) -> L211.GradeCost


    # We need to add resource to cover historical production since it is not included in the supply curves
    # Do not include the first year for cumulative production since the model does not consider it
    L111.gas_prod_state_T_Yh_EJ %>%
      mutate(grade = "grade.hist") %>%
      group_by(state, depresource, subresource, grade) %>%
      mutate(timestep = year - lag(year, n = 1L)) %>%
      filter(timestep != is.na(timestep)) %>%
      mutate(value = prod * timestep) %>%
      summarise(available = sum(value)) %>%
      ungroup() -> L211.CumulHistProduction

    # Merge costs and available
    # Sort by costs while grouping by state and resource to get grades in an appropriate order
    L111.gas_supply_state_T_EJ %>%
      mutate(grade.max = 0) %>%
      rename(depresource = resource) %>%
      mutate(subresource = depresource) %>%
      gather(grade, available, -state, -depresource, -subresource) -> L211.GradeAvail

    L111.unconv_gas_supply_state_EJ %>%
      select(state, resource, grade, available) %>%
      rename(depresource = resource) %>%
      mutate(subresource = depresource) -> L211.GradeAvail.unconv

    L211.GradeAvail %>% bind_rows(L211.CumulHistProduction, L211.GradeAvail.unconv) %>%
      left_join_error_no_match(L211.GradeCost, by = c("state", "depresource", "grade")) %>%
      mutate(available = round(available, 7),
             cost = round(cost, 3)) %>%
      arrange(state, depresource, cost) -> L211.Grades

    # Create state regional natural gas sectors which aggregate resource types.
    # L211.Sector: Regional natural gas sector to aggregate gas types.
    L111.gas_supply_state_T_EJ %>%
      distinct(state) -> L211.gas_regions

    # TODO: these logit assumptions matter
    L211.gas_regions %>%
      rename(region = state) %>%
      mutate(supplysector = L211.gas_prod_sector_name,
             output.unit = "EJ",
             input.unit = "EJ",
             price.unit = "1975$/GJ",
             logit.year.fillout = MODEL_BASE_YEARS[1],
             logit.exponent = -3,
             logit.type = NA) -> L211.Sector

    L211.Supplysector_NGprod %>% bind_rows(L211.Sector) %>%
      select(LEVEL2_DATA_NAMES[["Supplysector"]]) %>%
      mutate(logit.type = NA) -> L211.Sector

    # TODO
    # L211.SectorLogitTables <- get_logit_fn_tables( L211.Sector, names_SupplysectorLogitType,
    #                                                base.header="Supplysector_", include.equiv.table=T, write.all.regions=F )

    # Add USA & state natural gas production sectors to CO2 Coefs, format for output
    L211.PrimaryCO2Coef %>%
      bind_rows(L211.Sector %>%
                  select(region) %>%
                  rename(state = region) %>%
                  mutate(resource = L211.gas_prod_sector_name,
                         co2.coef = 14.2)) %>%
      rename(region = state, PrimaryFuelCO2Coef.name = resource, PrimaryFuelCO2Coef = co2.coef )-> L211.PrimaryCO2Coef

    # L211.Subsector: Regional natural gas subsector to aggregate gas types
    # NOTE: these logit assumptions do not matter as there is no competition at this nest
    L111.gas_supply_state_T_EJ %>%
      select(state, resource) %>%
      bind_rows(L111.unconv_gas_supply_state_EJ %>%
                  select(state, resource)) %>%
      distinct(state, resource) %>%
      rename(region = state) %>%
      rename(subsector = resource) %>%
      mutate(supplysector = L211.gas_prod_sector_name,
             logit.year.fillout = MODEL_BASE_YEARS[1],
             logit.exponent=-6,
             logit.type=NA) %>%
      select(region, supplysector, subsector, logit.year.fillout, logit.exponent, logit.type) -> L211.Subsector

    # TODO
    # L211.SubsectorLogitTables <- get_logit_fn_tables( L211.Subsector, names_SubsectorLogitType,
    #                                                   base.header="SubsectorLogit_", include.equiv.table=F, write.all.regions=F )


    # L211.SubsInterpRule: Regional natural gas subsector interpolation rules
    # Shareweight defaults to zero, which will get reset by tech cal if appropriate
    L211.Subsector %>%
      select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
      mutate(year = MODEL_BASE_YEARS[1],
             share.weight = 0) -> L211.SubsShrwtFlt

    # Warning: we should partition this table in two if to.value is not NA
    L211.Subsector %>%
      select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
      left_join(A10.subsector_interp, by = c("supplysector", "subsector")) %>%
      set_years() %>%
      mutate(from.year = as.character(from.year),
             to.year = as.character(to.year)) %>%
      select(LEVEL2_DATA_NAMES[["SubsectorInterp"]]) -> L211.SubsInterpRule

    #Create L211.Subs_shrwt for those states WITH unconventional resources
    L211.SubsInterpRule %>%
      filter(subsector == "unconventional gas other") %>%
      select(region) %>%
      distinct(region)-> Unconventional_States

    A10.subsector_shrwt %>%
      filter(region == "USA") %>%
      write_to_all_states(names(A10.subsector_shrwt)) %>%
      inner_join(Unconventional_States, by = c("region")) -> L211.Subs_shrwt

    # L211.GlobalDBTechInput: Pass through tech
    L211.Subsector %>%
      distinct(supplysector,subsector) %>%
      mutate(technology = subsector) %>%
      repeat_add_columns(tibble("year" = MODEL_YEARS)) -> L211.GlobalDBTechInput

    L211.GlobalDBTechInput %>%
      mutate(share.weight = if_else(year <= MODEL_FINAL_BASE_YEAR, 0, 1)) -> L211.GlobalDBTechShrwt

    L211.GlobalDBTechInput %>%
      mutate(minicam.energy.input = technology,
             coefficient = 1) -> L211.GlobalDBTechCoef

    # "L211.TechCal: natural gas resource calibration"
    L111.gas_prod_state_T_Yh_EJ -> L211.gas_prod_state_T_Th_EJ

    L211.gas_prod_state_T_Th_EJ %>%
      filter(prod > 0) %>%
      rename(region = state) %>%
      mutate(supplysector = L211.gas_prod_sector_name,
             subsector = depresource,
             stub.technology = depresource,
             minicam.energy.input = depresource,
             calibrated.value = round(prod, 7),
             share.weight.year = year,
             subs.share.weight = 1,
             tech.share.weight = 1) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCalInput"]]) -> L211.TechCal

    # L211.TechStubs: empty stubs for state / gas type that did not produce historically"
    L211.Subsector %>%
      anti_join(L211.TechCal, by = c("region", "subsector")) %>%
      select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
      mutate(technology = subsector) -> L211.TechStubs

    # Add these resources to the traded natural gas sector
    # "L211.TNGSubsInterp: The interpolation rule for the regions in the traded natural gas sector."
    L211.regional_ng_sector <- "natural gas production"

    L211.gas_prod_state_T_Th_EJ %>% group_by(state, year) %>%
      summarise(value = sum(prod)) %>%
      ungroup(state) -> L211.gas_prod_state_Yh_EJ

    L211.gas_prod_state_Yh_EJ %>%
      distinct(state) %>%
      mutate(region = "USA",
             supplysector = L211.regional_ng_sector,
             subsector = paste(state, L211.gas_prod_sector_name),
             apply.to = "share-weight",
             from.year = as.character(MODEL_FINAL_BASE_YEAR),
             to.year = as.character(MODEL_YEARS[ length( MODEL_YEARS ) ]),
             interpolation.function="fixed" ) %>%
      select(-state) -> L211.TNGSubsInterp

    L211.SubsInterpRule %>%
      bind_rows(L211.TNGSubsInterp) -> L211.SubsInterpRule

    # L211.TNGSubsectorLogit: The subsector logits for the regions in the traded natural gas sector.
    # NOTE: these logit assumptions do not matter as there is no competition at this nest
    L211.TNGSubsInterp %>%
      mutate(logit.year.fillout = MODEL_BASE_YEARS[1],
             logit.exponent = -6,
             logit.type = NA) %>%
      select(LEVEL2_DATA_NAMES[["SubsectorLogit"]]) -> L211.TNGSubsectorLogit

    # TODO
    # L211.TNGSubsectorLogitTables <- get_logit_fn_tables( L211.TNGSubsectorLogit, names_SubsectorLogitType,
    #                                                      base.header="SubsectorLogit_", include.equiv.table=F, write.all.regions=F )

    # TODO
    # for( logit.table in names( L211.TNGSubsectorLogitTables ) ) {
    #   if( nrow( L211.TNGSubsectorLogitTables[[ logit.table ]]$data ) > 0 ) {
    #     L211.SubsectorLogitTables[[ logit.table ]]$data <- rbind( L211.SubsectorLogitTables[[ logit.table ]]$data,
    #                                                               L211.TNGSubsectorLogitTables[[ logit.table ]]$data )
    #   }
    # }

    # "L211.TNGTech: create the tech to simply pass through state production"
    L211.gas_prod_state_Yh_EJ %>%
      rename(market.name = state) %>%
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

    # "L211.DepresourceCal: subresource calproduction"
    L211.TechCal %>%
      rename(depresource = subsector) %>%
      rename(subresource = stub.technology) %>%
      rename(cal.production = calibrated.value) %>%
      select(region, depresource, subresource, year, cal.production) -> L211.DepresourceCal


    # ===================================================
    # Produce outputs

    L211.PrimaryCO2Coef %>%
      add_title("USA and state natural gas production carbon Coefs") %>%
      add_units("kg C per GJ") %>%
      add_comments("USA and state natural gas production carbon Coefs") %>%
      add_precursors("L111.gas_supply_state_T_EJ",
                     "L111.unconv_gas_supply_state_EJ") ->
      L211.PrimaryCO2Coef

    L211.Depresource %>%
      add_title("resource parameters") %>%
      add_units("output unit EJ price unit 1975$ per GJ") %>%
      add_comments("resource parameters") %>%
      same_precursors_as("L211.PrimaryCO2Coef") ->
      L211.Depresource

    L211.DepresourcePrice %>%
      add_title("add a resource price in initial base year") %>%
      add_units("TODO") %>%
      add_comments("add a resource price in initial base year") %>%
      same_precursors_as("L211.PrimaryCO2Coef") ->
      L211.DepresourcePrice

    L211.DepresourceCal %>%
      add_title("subresource calibrated production") %>%
      add_units("EJ") %>%
      add_comments("subresource calibrated production") %>%
      add_precursors("L111.gas_supply_state_T_EJ") ->
      L211.DepresourceCal

    L211.DepresourceTechChange %>%
      add_title("subresource tech change") %>%
      add_units("unitless") %>%
      add_comments("subresource tech change") %>%
      same_precursors_as("L211.PrimaryCO2Coef") %>%
      add_precursors("gcam-usa/A10.TechChange") ->
      L211.DepresourceTechChange

    L211.Grades %>%
      add_title("resource curve") %>%
      add_units("unitless") %>%
      add_comments("resource curve") %>%
      add_precursors("gcam-usa/ETSAP_gas_cost_range",
                     "gcam-usa/BOEM_gas_cost",
                     "L111.unconv_gas_supply_state_EJ",
                     "L111.gas_prod_state_T_Yh_EJ",
                     "L111.gas_supply_state_T_EJ") ->
      L211.Grades

    L211.Sector %>%
      add_title("supplysector logit table") %>%
      add_units("unitless") %>%
      add_comments("supplysector logit table") %>%
      add_precursors("gcam-usa/A23.gas_sector_vertical",
                     "L111.gas_supply_state_T_EJ") ->
      L211.Sector


    L211.Subsector %>%
      add_title("subresource logit table") %>%
      add_units("unitless") %>%
      add_comments("subresource logit table") %>%
      add_precursors("L111.unconv_gas_supply_state_EJ",
                     "L111.gas_supply_state_T_EJ") ->
      L211.Subsector

    L211.SubsShrwtFlt %>%
      add_title("subsector shareweight table") %>%
      add_units("unitless") %>%
      add_comments("subsector shareweight table") %>%
      same_precursors_as("L211.Subsector") ->
      L211.SubsShrwtFlt

    L211.SubsInterpRule %>%
      add_title("subsector interpolation rule") %>%
      add_units("unitless") %>%
      add_comments("subsector interpolation rule") %>%
      same_precursors_as("L211.Subsector") %>%
      add_precursors("gcam-usa/A10.subsector_interp") ->
      L211.SubsInterpRule

    L211.Subs_shrwt %>%
      add_title("subsector shareweight in future years") %>%
      add_units("unitless") %>%
      add_comments("subsector shareweight in future years") %>%
      same_precursors_as("L211.SubsInterpRule") %>%
      add_precursors("gcam-usa/A10.subsector_shrwt") ->
      L211.Subs_shrwt

    L211.GlobalDBTechShrwt %>%
      add_title("global technology database technology shareweight") %>%
      add_units("unitless") %>%
      add_comments("global technology database technology shareweight") %>%
      same_precursors_as("L211.Subsector") ->
      L211.GlobalDBTechShrwt

    L211.GlobalDBTechCoef %>%
      add_title("global technology database technology coefficient") %>%
      add_units("unitless") %>%
      add_comments("global technology database technology coefficient") %>%
      same_precursors_as("L211.Subsector") ->
      L211.GlobalDBTechCoef

    L211.TechCal %>%
      add_title("natural gas resource technology calibration") %>%
      add_units("EJ") %>%
      add_comments("natural gas resource technology calibration") %>%
      add_precursors("L111.gas_prod_state_T_Yh_EJ") ->
      L211.TechCal

    L211.TechStubs %>%
      add_title("natural gas resource stub.technology table") %>%
      add_units("unitless") %>%
      add_comments("natural gas resource stub.technology table") %>%
      same_precursors_as("L211.Subsector") ->
      L211.TechStubs

    L211.TNGTechProduction %>%
      add_title("create the tech to simply pass through state production to USA and calibration technology production") %>%
      add_units("unitless") %>%
      add_comments("create the tech to simply pass through state production to USA and calibration technology production") %>%
      add_precursors("L111.gas_prod_state_T_Yh_EJ") ->
      L211.TNGTechProduction

    L211.TNGTechCoef %>%
      add_title("create the tech to simply pass through state production to USA and set coefficient as 1") %>%
      add_units("unitless") %>%
      add_comments("create the tech to simply pass through state production to USA and set coefficient as 1") %>%
      add_precursors("L111.gas_prod_state_T_Yh_EJ") ->
      L211.TNGTechCoef

    L211.InterestRate_Offshore %>%
      add_title("dummy interest rate for offshore regions") %>%
      add_units("unitless") %>%
      add_comments("dummy interest rate for offshore regions") %>%
      add_precursors("gcam-usa/BOEM_gas_cost") ->
      L211.InterestRate_Offshore

    L211.Pop_Offshore %>%
      add_title("dummy population for offshore regions") %>%
      add_units("unitless") %>%
      add_comments("dummy population for offshore regions") %>%
      same_precursors_as("L211.InterestRate_Offshore") ->
      L211.Pop_Offshore

    L211.BaseGDP_Offshore %>%
      add_title("dummy baseGDP for offshore regions") %>%
      add_units("unitless") %>%
      add_comments("dummy baseGDP for offshore regions") %>%
      same_precursors_as("L211.InterestRate_Offshore") ->
      L211.BaseGDP_Offshore

    L211.LaborForceFillout_Offshore %>%
      add_title("dummy laborforce for offshore regions") %>%
      add_units("unitless") %>%
      add_comments("dummy laborforce for offshore regions") %>%
      same_precursors_as("L211.InterestRate_Offshore") ->
      L211.LaborForceFillout_Offshore

    return_data(L211.PrimaryCO2Coef,
                L211.Depresource,
                L211.DepresourcePrice,
                L211.DepresourceCal,
                L211.DepresourceTechChange,
                L211.Grades,
                L211.Sector,
                L211.Subsector,
                L211.SubsShrwtFlt,
                L211.SubsInterpRule,
                L211.Subs_shrwt,
                L211.GlobalDBTechShrwt,
                L211.GlobalDBTechCoef,
                L211.TechCal,
                L211.TechStubs,
                L211.TNGTechProduction,
                L211.TNGTechCoef,
                L211.InterestRate_Offshore,
                L211.Pop_Offshore,
                L211.BaseGDP_Offshore,
                L211.LaborForceFillout_Offshore)
  } else {
    stop("Unknown command")
  }
}
