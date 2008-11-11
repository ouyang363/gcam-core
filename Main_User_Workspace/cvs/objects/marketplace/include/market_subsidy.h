#ifndef _MARKET_SUBSIDY_H_
#define _MARKET_SUBSIDY_H_
#if defined(_MSC_VER_)
#pragma once
#endif

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
 * All rights to use the Software are granted on condition that such
 * rights are forfeited if User fails to comply with the terms of
 * this Agreement.
 * 
 * User agrees to identify, defend and hold harmless BATTELLE,
 * its officers, agents and employees from all liability involving
 * the violation of such Export Laws, either directly or indirectly,
 * by User.
 */


/*! 
* \file market_subsidy.h
* \ingroup Objects
* \brief The MarketPortfolioStandard class header file.
* \author Sonny Kim
*/

#include "marketplace/include/market.h"

/*!
* \ingroup Objects
* \brief A class which defines a MarketSubsidy object which is used for fuel subsidies. 
* \author Sonny Kim
*/

class MarketSubsidy: public Market {
public:
    MarketSubsidy( const std::string& goodNameIn, const std::string& regionNameIn, const int periodIn );
    virtual IMarketType::Type getType() const;

    virtual void initPrice();
    virtual void setPriceFromLast( const double lastPrice );

    virtual void nullDemand();

    virtual bool meetsSpecialSolutionCriteria() const;
    virtual bool shouldSolve() const;
    virtual bool shouldSolveNR() const;
protected:
    virtual void toDebugXMLDerived( std::ostream& out, Tabs* tabs ) const;
};

#endif // _MARKET_SUBSIDY_H_