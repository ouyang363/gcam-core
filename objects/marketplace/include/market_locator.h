#ifndef _MARKET_LOCATOR_H_
#define _MARKET_LOCATOR_H_
#if defined(_MSC_VER_)
#pragma once
#endif

/*! 
* \file market_locator.h
* \ingroup Objects
* \brief The MarketLocator class header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include <string>
#include <memory>
#include <boost/shared_ptr.hpp>
template <class T, class U> class HashMap;

#include <boost/functional/hash/hash.hpp>

/*!
* \ingroup Objects
* \brief This class is responsible for rapidly looking up the location of a
*        market within the marketplace given region and good names.
* \details This class is designed to efficiently lookup the location of a market
*          within the Marketplace given the region of the object requesting the
*          lookup, and the name of the good they want to lookup information
*          about. This object is setup initially by the Marketplace through
*          calls to Marketplace::addMarket. During this function, the
*          Marketplace gives the MarketLocator the name of a market area, a
*          region, a good name, and a lookup number to use if the MarketLocator
*          does not already know the location of the market. The MarketLocator
*          stores this information in a pair of list. The first list contains
*          nodes which represent market areas. These nodes each store a list of
*          sectors and their corresponding sector numbers. This first list is
*          only used during the market creation process. The second list stored
*          by the MarketLocator is a list of nodes representing regions, each
*          containing a list of sectors and their market numbers. This is the
*          list which is used to determine a market number from a region name
*          and good name throughout the model run.
* \author Josh Lurz
*/
class MarketLocator
{
public:
	MarketLocator();
	~MarketLocator();
	int addMarket( const std::string& aMarket, const std::string& aRegion, const std::string& aGoodName,
		const int aUniqueNumber );
	int getMarketNumber( const std::string& aRegion, const std::string& aGoodName ) const;

	//! An identifier returned by the various functions if the market does not
	//! exist.
	static const int MARKET_NOT_FOUND = -1;
private:
	int getMarketNumberInternal( const std::string& aRegion, const std::string& aGoodName ) const;

	/*! \brief A single node in a list of goods which contains the name of the
	*          good and its market location.
	*/
	class GoodNode {
	public:
		GoodNode( const std::string& aName, int aMarketNumber );

		//! The good name.
		const std::string mName;

		//! The market number.
		const int mNumber;
	};

	/*! \brief A single node in a list of Regions or Markets which contains the
	*          name of the Region or Market and a list of good names and market
	*          locations. 
	*/
	class RegionOrMarketNode {
	public:
		RegionOrMarketNode( const std::string& aName );
		~RegionOrMarketNode();
		inline const std::string& getName() const;
		int addGood( const std::string& aGoodName, const int aMarketNumber );
		int getMarketNumber( const std::string& aGoodName ) const;
	private:
		//! The type of the list that contains the goods.
		typedef HashMap<std::string, boost::shared_ptr<GoodNode> > SectorNodeList;

		//! A list of sectors contained by this market or region.
		std::auto_ptr<SectorNodeList> mSectorNodeList;
		
		//! The region or market area name.
		const std::string mName;
	};

	//! The type of the lists of regions or markets.
	typedef HashMap<std::string, boost::shared_ptr<RegionOrMarketNode> > RegionMarketList;

	//! A pointer to the last region looked up.
	mutable const RegionOrMarketNode* mLastRegionLookup;

	//! A list of market areas each containing a list of sectors contained by
	//! the market.
	std::auto_ptr<RegionMarketList> mMarketList;

	//! A list of regions each containing a list of of sectors contained by the
	//! region.
	std::auto_ptr<RegionMarketList> mRegionList;
};

// Inline definitions.
/*! \brief Get the name of the RegionOrMarketNode.
* \return The name of the RegionOrMarketNode.
*/
const std::string& MarketLocator::RegionOrMarketNode::getName() const {
	return mName;
}

#endif // _MARKET_LOCATOR_H_
