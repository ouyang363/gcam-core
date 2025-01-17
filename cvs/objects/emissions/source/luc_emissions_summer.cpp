/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
* CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
* LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
* sentence must appear on any copies of this computer software.
* 
* EXPORT CONTROL
* User agrees that the Software will not be shipped, transferred or
* exported into any country or used in any manner prohibited by the
* United States Export Administration Act or any other applicable
* export laws, restrictions or regulations (collectively the "Export Laws").
* Export of the Software may require some form of license or other
* authority from the U.S. Government, and failure to obtain such
* export control license may result in criminal liability under
* U.S. laws. In addition, if the Software is identified as export controlled
* items under the Export Laws, User represents and warrants that User
* is not a citizen, or otherwise located within, an embargoed nation
* (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
*     and that User is not otherwise prohibited
* under the Export Laws from receiving the Software.
* 
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
* 
* For further details, see: http://www.globalchange.umd.edu/models/gcam/
*
*/


/*! 
* \file emissions_summer.cpp
* \ingroup Objects
* \brief The LUCEmissionsSummer class source file.
*
* \author Kate Calvin
*/

#include "util/base/include/definitions.h"
#include <cassert>
#include "emissions/include/luc_emissions_summer.h"
#include "ccarbon_model/include/icarbon_calc.h"
#include "climate/include/magicc_model.h"

using namespace std;

/*! \brief Constructor
* \param aGHG GHG that is being summed.
*/
LUCEmissionsSummer::LUCEmissionsSummer( const string& aGHGName ):
    mGHGName( aGHGName ),
    mPositiveEmissionsByYear( scenario->getModeltime()->getStartYear(), scenario->getModeltime()->getEndYear() ),
    mNegativeEmissionsByYear( scenario->getModeltime()->getStartYear(), scenario->getModeltime()->getEndYear() ),
    mBelowTotalEmissionsByYear( scenario->getModeltime()->getStartYear(), scenario->getModeltime()->getEndYear() )

{
}

void LUCEmissionsSummer::startVisitCarbonCalc( const ICarbonCalc* aCarbonCalc,
                                               const int aPeriod )
{
    const int currYear = scenario->getModeltime()->getper_to_yr( aPeriod );
    // Add land use change emissions.
    if( mGHGName == "CO2NetLandUse" ){
        const int startYear = currYear - scenario->getModeltime()->gettimestep( aPeriod ) + 1;
        for ( int year = startYear; year <= currYear; year++ ) {
            // note that ICarbonCalc keeps track of net above LUC emissions
            // and gross positive above LUC emissions
            // thus we will back out the gross negative above from these
            double netAboveLUC = aCarbonCalc->getNetLandUseChangeEmissionAbove( year );
            double belowLUC = aCarbonCalc->getNetLandUseChangeEmissionBelow( year );
            double grossPositiveAboveLUC = aCarbonCalc->getGrossPositiveLandUseChangeEmissionAbove( year );
            double grossNegativeAbove = netAboveLUC - grossPositiveAboveLUC;
            mPositiveEmissionsByYear[ year ] += grossPositiveAboveLUC;
            mNegativeEmissionsByYear[ year ] += grossNegativeAbove;
            mBelowTotalEmissionsByYear[ year ] += belowLUC;
        }
    }
}

/*! \brief Get the net LUC emissions sum.
 * \param aYear Model year for which to get emissions.
 * \return The emissions sum.
 */
double LUCEmissionsSummer::getEmissions( const int aYear ) const {
    // The value may not be initialized if there were no GHGs, or no AgLU for
    // net land use change emissions. The default zero will be correct though.

    // The emissions sum may be negative if uptake is occurring.
    return mPositiveEmissionsByYear[ aYear ] + mNegativeEmissionsByYear[ aYear ] + mBelowTotalEmissionsByYear[ aYear ];
}

/*! \brief Get the gross positive LUC emissions sum.
 * \param aYear Model year for which to get emissions.
 * \return The positive emissions sum.
 */
double LUCEmissionsSummer::getEmissionsPositive( const int aYear ) const {
    // The value may not be initialized if there were no GHGs, or no AgLU for
    // net land use change emissions. The default zero will be correct though.

    // The emissions sum may be negative if uptake is occurring.
    double belowPosEmiss = std::max(mBelowTotalEmissionsByYear[ aYear ].get(), 0.0);
    return mPositiveEmissionsByYear[ aYear ] + belowPosEmiss;
}

/*! \brief Get the gross negative LUC emissions sum.
 * \details The value returned is akin to uptake but negative.
 * \param aYear Model year for which to get emissions.
 * \return The emissions sum.
 */
double LUCEmissionsSummer::getEmissionsNegative( const int aYear ) const {
    // The value may not be initialized if there were no GHGs, or no AgLU for
    // net land use change emissions. The default zero will be correct though.

    // The emissions sum may be negative if uptake is occurring.
    double belowNegEmiss = std::min(mBelowTotalEmissionsByYear[ aYear ].get(), 0.0);
    return mNegativeEmissionsByYear[ aYear ] + belowNegEmiss;
}

/*! \brief Return whether any emissions were set for the year.
 * \param aYear Model year.
 * \return Whether any emissions were set.
 */
double LUCEmissionsSummer::areEmissionsSet( const int aYear ) const {
    return mPositiveEmissionsByYear[ aYear ].isInited() || mNegativeEmissionsByYear[ aYear ].isInited();
}
