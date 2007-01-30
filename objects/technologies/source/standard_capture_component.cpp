/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Laboratory operated by Battelle Memorial Institute.
 * Battelle has certain unperfected rights in the software which should not be
 * copied or otherwise disseminated outside your organization without the
 * express written authorization from Battelle. All rights to the software are
 * reserved by Battelle. Battelle makes no warranty, express or implied, and
 * assumes no liability or responsibility for the use of this software.
 */

/*! 
 * \file standard_capture_component.cpp
 * \ingroup Objects
 * \brief StandardCaptureComponent source file.
 * \author Josh Lurz
 */

#include "util/base/include/definitions.h"
#include <string>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>
#include "util/base/include/xml_helper.h"
#include "technologies/include/standard_capture_component.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/scenario.h"
#include "util/logger/include/ilogger.h"
#include "containers/include/dependency_finder.h"

using namespace std;

extern Scenario* scenario;

//! Constructor
StandardCaptureComponent::StandardCaptureComponent():
mSequesteredAmount( scenario->getModeltime()->getmaxper() ),
mRemoveFraction( 0 ),
mStorageCost( 0 ),
mEfficiencyPenalty( 0 ),
mNonEnergyCostPenalty( 0 )
{
}


StandardCaptureComponent* StandardCaptureComponent::clone() const {
    return new StandardCaptureComponent( *this );
}

bool StandardCaptureComponent::isSameType( const string& aType ) const {
    return aType == getXMLNameStatic();
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
* \details This public function accesses the private constant string, XML_NAME.
*          This way the tag is always consistent for both read-in and output and
*          can be easily changed. The "==" operator that is used when parsing,
*          required this second function to return static.
* \note A function cannot be static and virtual.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME as a static.
*/
const string& StandardCaptureComponent::getXMLNameStatic() {
    const static string XML_NAME = "standard-capture-component";
    return XML_NAME;
}

const string& StandardCaptureComponent::getName() const {
    return getXMLNameStatic();
}

// Documentation inherits.
bool StandardCaptureComponent::XMLParse( const xercesc::DOMNode* node ){
    /*! \pre Assume we are passed a valid node. */
    assert( node );

    const xercesc::DOMNodeList* nodeList = node->getChildNodes();
    for( unsigned int i = 0; i < nodeList->getLength(); i++ ) {
        const xercesc::DOMNode* curr = nodeList->item( i );
        if( curr->getNodeType() != xercesc::DOMNode::ELEMENT_NODE ){
            continue;
        }
        const string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );
        if( nodeName == "storage-market" ){
            mStorageMarket = XMLHelper<string>::getValue( curr );
        }
        // TODO: Fix this on commit.
        else if( nodeName == "remove-fraction" || nodeName == "removefrac" ){
            mRemoveFraction = XMLHelper<double>::getValue( curr );
        }
        // TODO: Fix this on commit.
        else if( nodeName == "storage-cost" || nodeName == "storageCost" ){
            mStorageCost = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "efficiency-penalty" ){
            mEfficiencyPenalty = XMLHelper<double>::getValue( curr );
        }
        // TODO: Fix this on commit.
        else if( nodeName == "non-energy-penalty" || nodeName == "neCostPenalty" ){
            mNonEnergyCostPenalty = XMLHelper<double>::getValue( curr );
        }
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            mainLog << "Unknown tag " << nodeName << " encountered while processing " << getXMLNameStatic() << endl;
        }
    }

    // TODO: Handle success and failure better.
    return true;
}

void StandardCaptureComponent::toInputXML( ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );
    XMLWriteElementCheckDefault( mStorageMarket, "storage-market", aOut, aTabs, string( "" ) );
    XMLWriteElementCheckDefault( mRemoveFraction, "remove-fraction", aOut, aTabs, 0.0 );
    XMLWriteElementCheckDefault( mStorageCost, "storage-cost", aOut, aTabs, util::getLargeNumber() );
    XMLWriteElementCheckDefault( mEfficiencyPenalty, "efficiency-penalty", aOut, aTabs, 0.0 );
    XMLWriteElementCheckDefault( mNonEnergyCostPenalty, "non-energy-penalty", aOut, aTabs, 0.0 );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

void StandardCaptureComponent::toDebugXML( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );
    XMLWriteElement( mStorageMarket, "storage-market", aOut, aTabs );
    XMLWriteElement( mRemoveFraction, "remove-fraction", aOut, aTabs );
    XMLWriteElement( mStorageCost, "storage-cost", aOut, aTabs );
    XMLWriteElement( mEfficiencyPenalty, "efficiency-penalty", aOut, aTabs );
    XMLWriteElement( mNonEnergyCostPenalty, "non-energy-penalty", aOut, aTabs );
    XMLWriteElement( mSequesteredAmount[ aPeriod ], "sequestered-amount", aOut, aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

void StandardCaptureComponent::completeInit( const string& aRegionName,
                                             const string& aSectorName,
                                             DependencyFinder* aDependencyFinder )
{
    // Add the storage market as a dependency of the sector. This is because
    // this sector will have to be ordered first so that the total demand and
    // price for storage are known.
    if( aDependencyFinder ){
        aDependencyFinder->addDependency( aSectorName, mStorageMarket );
    }
}

void StandardCaptureComponent::initCalc( const string& aRegionName,
                                         const string& aSectorName,
                                         const string& aFuelName,
                                         const int aPeriod )
{
}

double StandardCaptureComponent::getStorageCost( const string& aRegionName, const int aPeriod ) const {
    // Check if there is a market for storage.
    double storageMarketPrice = scenario->getMarketplace()->getPrice( mStorageMarket,
                                                                      aRegionName,
                                                                      aPeriod, false );
    double storageCost;
    if( storageMarketPrice == Marketplace::NO_MARKET_PRICE ){
        // There is no carbon market. Use the read-in cost.
        storageCost = mStorageCost;
    }
    else {
        // Use the market cost.
        storageCost = storageMarketPrice;
    }
    return storageCost;
}

double StandardCaptureComponent::getRemoveFraction() const {
    return mRemoveFraction;
}

void StandardCaptureComponent::calcSequesteredAmount( const string& aRegionName,
                                                      const string& aGHGName,
                                                      const double aInput,
                                                      const double aOutput,
                                                      const double aInputCoef,
                                                      const double aOutputCoef,
                                                      const int aPeriod )
{
    // Currently sequestration objects should only be used for CO2.
    assert( aGHGName == "CO2" );

    // Calculate the amount as the removal fraction multiplied by the difference
    // between input and output emissions.
    mSequesteredAmount[ aPeriod ] = mRemoveFraction * ( ( aInput * aInputCoef ) 
                                    - ( aOutput * aOutputCoef ) );
    
    // Add the demand to the marketplace.
    if( mSequesteredAmount[ aPeriod ] > 0 ){
        // set sequestered amount as demand side of carbon storage market
        Marketplace* marketplace = scenario->getMarketplace();
        marketplace->addToDemand( mStorageMarket, aRegionName, mSequesteredAmount[ aPeriod ],
                                  aPeriod, false );
    }
}

double StandardCaptureComponent::getSequesteredAmount( const string& aGHGName,
                                                       const bool aGetGeologic,
                                                       const int aPeriod ) const 
{
    // Only return emissions if the type of the sequestration equals is geologic.
    // TODO: Determine how/if to handle non-co2 GHGs.
    if( aGetGeologic && aGHGName == "CO2" ){
        return mSequesteredAmount[ aPeriod ];
    }
    return 0;
}

double StandardCaptureComponent::getEffectiveEfficiency( const double aTechnologyEfficiency,
                                                         const int aPeriod ) const
{
    // Technology efficiency must be greater than zero.
    assert( aTechnologyEfficiency > 0 );

    // Calculate effective efficiency, reduces the efficiency by a penalty.
    return aTechnologyEfficiency * ( 1 - mEfficiencyPenalty );
}

double StandardCaptureComponent::getTotalNonEnergyCost( const double aTechnologyEfficiency,
                                                        const double aTechnologyNonEnergyCost,
                                                        const int aPeriod ) const 
{
    // Technology non-energy cost must be positive for a penalty to be applied. 
    assert( aTechnologyNonEnergyCost > 0 );

    // Check for a valid efficiency.
    assert( aTechnologyEfficiency > 0 );

    return aTechnologyNonEnergyCost * ( 1 + mNonEnergyCostPenalty );
}
