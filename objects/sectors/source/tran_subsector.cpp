/*! 
* \file tran_subsector.cpp
* \ingroup Objects
* \brief transporation technology class source file.
* \author Sonny Kim, Josh Lurz, Steve Smith, Marshall Wise
*/

#include "util/base/include/definitions.h"
#include <string>
#include <iostream>
#include <cassert>
#include <vector>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>

#include "sectors/include/tran_subsector.h"
#include "technologies/include/tran_technology.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "containers/include/info_factory.h"
#include "containers/include/iinfo.h"
#include "util/base/include/xml_helper.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/summary.h"
#include "containers/include/gdp.h"
#include "demographics/include/demographic.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;
const string TranSubsector::XML_NAME = "tranSubsector";

/*  Begin TranSubsector Method Definitions */

/*! \brief Default constructor for TranSubsector.
*
* Default constructor takes region name, sector name and units as arguments
* and resizes and initializes vectors.
* \param regionName The name of the region.
* \param sectorName The name of the sector.
* \param aUnit The sector output unit.
* \author Josh Lurz, Sonny Kim
*/
TranSubsector::TranSubsector( const string& regionName, const string& sectorName ): Subsector( regionName, sectorName ) {
	// resize vectors
	const Modeltime* modeltime = scenario->getModeltime();
	const int maxper = modeltime->getmaxper();
	speed.resize( maxper ); // average speed of mode
	mPopulation.resize( maxper ); // copy of population since demog object not available
	popDenseElasticity.resize( maxper );
	popDensity = 1; // initialize to 1 for now
	mAddTimeValue = false; // initialize to false
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overridden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& TranSubsector::getXMLName() const {
    return XML_NAME;
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* The "==" operator that is used when parsing, required this second function to return static.
* \note A function cannot be static and virtual.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME as a static.
*/
const std::string& TranSubsector::getXMLNameStatic() {
    return XML_NAME;
}

/*! \brief Function Parses any input variables specific to derived classes.
* \param nodeName The name of the XML node.
* \param curr A pointer to the XML DOM node.
* \author Josh Lurz, Sonny Kim
* \return Boolean for node match.
*/
bool TranSubsector::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ) {    
	// additional read in for transportation
	const Modeltime* modeltime = scenario->getModeltime();
	if( nodeName == "addTimeValue" ){
		mAddTimeValue = XMLHelper<bool>::getValue( curr );
	}
	else if( nodeName == "speed" ){
		XMLHelper<double>::insertValueIntoVector( curr, speed, modeltime );
	}
	else if( nodeName == "popDenseElasticity" ){
		XMLHelper<double>::insertValueIntoVector( curr, popDenseElasticity, modeltime );
	}
	else {
		return false;
	}
	return true;
}

/*! \brief Returns true if the nodename is a valid child for this class.
*
* Virtual function which specifies the XML name of the possible technology children of this class.
* This function allows all technologies to be properly parsed using the base subsector code.
* \author Steve Smith
* \pre Needs corresponding createChild() function
* \return True if nodename is a valid child of this class.
*/
bool TranSubsector::isNameOfChild  ( const string& nodename ) const {
	return nodename == TranTechnology::getXMLNameStatic1D();
}

/*!
 * \brief Derived helper function to generate a child element or construct the
 *        appropriate technology.
 * \param aTechType The name of the XML node, which is the type of the
 *        technology.
 * \param aTechName The name of the new technology.
 * \param aYear The year of the new technology.
 * \pre isNameOfChild returned that the type could be created.
 * \author Steve Smith
 * \return A newly created technology of the specified type.
 */
ITechnology* TranSubsector::createChild( const string& aTechType,
                                         const string& aTechName,
                                         const int aTechYear ) const
{
    return new TranTechnology( aTechName, aTechYear );
}

/*! \brief XML output stream for derived classes
*
* Function writes output due to any variables specific to derived classes to XML
* \author Josh Lurz, Sonny Kim
* \param out reference to the output stream
* \param tabs A tabs object responsible for printing the correct number of tabs. 
*/
void TranSubsector::toInputXMLDerived( ostream& out, Tabs* tabs ) const {
	XMLWriteElementCheckDefault( mAddTimeValue, "addTimeValue", out, tabs, false );
    const Modeltime* modeltime = scenario->getModeltime();
    XMLWriteVector( speed, "speed", out, tabs, modeltime, 0.0 );
    XMLWriteVector( popDenseElasticity, "popDenseElasticity", out, tabs, modeltime, 0.0 );
    XMLWriteVector( mServiceOutputs, "serviceoutput", out, tabs, modeltime, 0.0 );
}

/*! \brief XML output for debugging.
* Function writes output to debugging XML
* \author Josh Lurz, Sonny Kim
* \param out reference to the output stream
* \param tabs A tabs object responsible for printing the correct number of tabs. 
*/
void TranSubsector::toDebugXMLDerived( const int period, ostream& out, Tabs* tabs ) const {
	XMLWriteElement( mAddTimeValue, "addTimeValue", out, tabs );
	XMLWriteElement( popDenseElasticity[ period ], "popDenseElasticity", out, tabs );
	XMLWriteElement( popDensity, "popDensity", out, tabs );
	XMLWriteElement( speed[ period ], "speed", out, tabs );
}

/*! \brief Perform any initializations needed for each period.
* \author Sonny Kim, Steve Smith, Josh Lurz
* \param aNationalAccount National accounts information
* \param aDemographics Regional demographics information
* \param aMoreSectorInfo Additional sector information required for subsector
* \param aPeriod Model period
*/
void TranSubsector::completeInit( const IInfo* aSectorInfo,
                                  DependencyFinder* aDependencyFinder,
                                  ILandAllocator* aLandAllocator,
                                  const GlobalTechnologyDatabase* aGlobalTechDB )
{
    // Only call base class completeInit.
    Subsector::completeInit( aSectorInfo, aDependencyFinder, aLandAllocator, aGlobalTechDB );
}

/*!
* \brief Perform any initializations needed for each period.
* \details Perform any initializations or calculations that only need to be done
*          once per period (instead of every iteration) should be placed in this
*          function.
* \warning The ghg part of this routine assumes the existence of technologies in
*          the previous and future periods
* \author Steve Smith, Sonny Kim
* \param aNationalAccount National accounts container.
* \param aDemographics Regional demographics container.
* \param aMoreSectorInfo sector info object.
* \param aPeriod Model period
*/
void TranSubsector::initCalc( NationalAccount* aNationalAccount,
							 const Demographic* aDemographics,
							 const MoreSectorInfo* aMoreSectorInfo,
							 const int aPeriod )
{
	// Check if illegal values have been read in
	if ( speed[ aPeriod ] <= 0 ) {
		speed[ aPeriod ] = 1;
		ILogger& mainLog = ILogger::getLogger( "main_log" );
		mainLog.setLevel( ILogger::ERROR );
		mainLog << "Speed was zero or negative in subsector: " << name << " in region " 
			<< regionName << ". Reset to 1." << endl;
	}
	// time in transit
	// initialize vector to hold population (thousands)
	// TODO: revise access to population to avoid statement below
	mPopulation[ aPeriod ] = aDemographics->getTotal( aPeriod );

	Subsector::initCalc( aNationalAccount, aDemographics, aMoreSectorInfo, aPeriod );
}

/*! \brief returns the subsector price.
* \details Calculates and returns share-weighted total price (subsectorprice)
*          with or without value of time.
* \author Sonny Kim
* \param aGDP Regional GDP object.
* \param aPeriod Model period
* \return The subsector price with or without value of time. 
*/
double TranSubsector::getPrice( const GDP* aGDP, const int aPeriod ) const {
	// mAddTimeValue is a boolean that determines whether the service price includes
	// the value of time
	if (mAddTimeValue) {
		return getGeneralizedPrice( aGDP, aPeriod );
	}
	// normal share-weighted total technology cost only
	return Subsector::getPrice( aGDP, aPeriod );
}

/*! \brief Get the time value for the period.
* \param aGDP The regional GDP container.
* \param aPeriod The model period.
* \author Sonny Kim
* \return The time value.
*/
double TranSubsector::getTimeValue( const GDP* aGDP, const int aPeriod ) const {
	const double WEEKS_PER_YEAR = 50;
	const double HOURS_PER_WEEK = 40;
	// calculate time value based on hours worked per year Convert GDPperCap
	// into dollars (instead of 1000's of $'s) GDP value at this point in the
	// code does not include energy feedback calculation for this year, so is,
	// therefore, approximate
	return aGDP->getApproxGDPperCap( aPeriod ) * 1000 / ( HOURS_PER_WEEK * WEEKS_PER_YEAR ) / speed[ aPeriod ];
}

/*! \brief Calculate the generalized service price for the mode that includes time value.
* \author Sonny Kim
* \param aGDP The regional GDP container.
* \param aPeriod The model period.
* \return The the generalized price.
*/
double TranSubsector::getGeneralizedPrice( const GDP* aGDP, const int aPeriod ) const {
	// add cost of time spent on travel by converting gdp/cap into an hourly
	// wage and multiplying by average speed.
	// The price unit is $ per service, e.g. $/pass-mi or $/ton-mi
	return Subsector::getPrice( aGDP, aPeriod ) + getTimeValue( aGDP, aPeriod );
}

/*! \brief Get the time in transit per day per person for the period.
*  Currently used for reporting only.
* \author Sonny Kim
* \param aPeriod The model period.
* \return The time in transit.
*/
double TranSubsector::getTimeInTransit( const int aPeriod ) const {
	const double DAYS_PER_YEAR = 365;
	const double POP_MILE_CONV = 1000;
	// calculate time in transit per day for each person using total population
	return getOutput( aPeriod ) / mPopulation[ aPeriod ] * POP_MILE_CONV  
		/ speed[ aPeriod ] / DAYS_PER_YEAR ;
}

/*! \brief Get service per day per capita for the period.
* \author Sonny Kim
* \param aPeriod The model period.
* \return The service per day per capita.
*/
double TranSubsector::getServicePerCapita( const int aPeriod ) const {
	const double DAYS_PER_YEAR = 365;
	const double POP_MILE_CONV = 1000;
	// units: million pass or ton mi / thousand persons
	return getOutput( aPeriod ) / mPopulation[ aPeriod ] * POP_MILE_CONV
		/ DAYS_PER_YEAR ;
}

/*! \brief Calculate and return subsector share.
* \author Sonny Kim
* \param aPeriod The model period.
* \param aGDP Regional GDP object.
* \return The subsector share.
*/
double TranSubsector::calcShare( const int aPeriod, const GDP* aGDP ) const {
	// Compute calibrating scaler.
	// The shares are based on the generalized cost of service that includes the 
	// value of time for both passenger and freight service
	double baseScaler;
	int basePeriod = 0;
    if( shrwts[ aPeriod ] > util::getSmallNumber() ){
		// assumes that subsector has output in the base period
		baseScaler = getOutput( basePeriod ) / shrwts[ basePeriod ] * pow( getPrice( aGDP, basePeriod ), -lexp[ basePeriod ] )
			* pow( aGDP->getBestScaledGDPperCap( basePeriod ), -fuelPrefElasticity[ basePeriod ] )
			* pow( popDensity, -popDenseElasticity[ basePeriod ] );
	}
	else {
		baseScaler = 1;
	}
	double share = baseScaler * shrwts[ aPeriod ] * pow( getPrice( aGDP, aPeriod ), lexp[ aPeriod ])
		* pow( aGDP->getBestScaledGDPperCap( aPeriod ), fuelPrefElasticity[ aPeriod ])
		* pow( popDensity, popDenseElasticity[ aPeriod ] );
	/*! \post The share must be greater than or equal to zero and valid. */
	assert( share >= 0 && util::isValidNumber( share ) );
	return share;
}

void TranSubsector::MCoutputAllSectors( const GDP* aGDP,
                                        const IndirectEmissionsCalculator* aIndirectEmissionsCalc,
                                        const vector<double> aSectorOutput ) const
{
	Subsector::MCoutputAllSectors( aGDP, aIndirectEmissionsCalc, aSectorOutput );
	// function protocol
	void dboutput4(string var1name,string var2name,string var3name,string var4name,
		string uname,vector<double> dout);
	const int maxPeriod = scenario->getModeltime()->getmaxper();
	const string& priceUnit = mSubsectorInfo->getString( "price-unit", true );
	vector<double> temp( maxPeriod );
	// Subsector timeValue price
	for( int per = 0; per < maxPeriod; ++per ){
		temp [ per ] = getTimeValue( aGDP, per );
	}
	dboutput4( regionName, "General", "TimeValue", sectorName + name, priceUnit, temp );
	// Subsector speed
	dboutput4( regionName, "General", "Speed", sectorName + name, "Miles/hr", speed );
	// time in transit (hours/day/person)
	for( int per = 0; per < maxPeriod; ++per ){
		temp[ per ] = getTimeInTransit( per );
	}
	dboutput4( regionName, "End-Use Service", sectorName+" TimeInTransit", name, "hrs/day/per", temp);
	// service per day per person (service/day/person)
	for( int per = 0; per < maxPeriod; ++per ){
		temp[ per ] = getServicePerCapita( per );
	}
	//dboutput4( regionName, "End-Use Service", sectorName+" ServicePerCapita", name, "tons/day/per", temp);

	// Do for all vehicle technologies in the TranSubsector
	// Write vehicle fuel economy in MPG for reporting only
	for( unsigned int i = 0; i < techs.size(); ++i ){
		const string subsecTechName = name + techs[i][0]->getName();
		for ( int per = 0; per < maxPeriod; ++per ) {
			// assumes vehicle intensity in Btu/veh-mi
			// convert intensity to MPG, 115400 Btu/gal of gasoline
			const double GASOLINE_NET_HEATCONTENT = 115400;
			temp[ per ] = GASOLINE_NET_HEATCONTENT / techs[i][per]->getIntensity( per );
		}
		dboutput4(regionName,"Tech Efficiency", sectorName, subsecTechName,"MPG",temp);
	}
}
