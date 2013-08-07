package ModelInterface.ModelGUI2.queries;

import ModelInterface.ModelGUI2.xmldb.XMLDB;
import ModelInterface.common.DataPair;

import javax.swing.JList;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.ListSelectionModel;

import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Iterator;
import java.util.Vector;
import java.util.LinkedHashMap;
import java.util.HashMap;
import java.util.EventListener;

import com.sleepycat.dbxml.XmlResults;
import com.sleepycat.dbxml.XmlValue;
import com.sleepycat.dbxml.XmlException;

public class DemandComponentsQueryBuilder extends QueryBuilder {
	/*
	public static Map varList;
	protected Map sectorList;
	protected Map subsectorList;
	protected Map techList;
	protected Map inputList;
	*/
	private static final String inputQueryPortion = "*[@type = 'input']";
	public static String xmlName = "demandComponentsQuery";
	public DemandComponentsQueryBuilder(QueryGenerator qgIn) {
		super(qgIn);
	}
	public EventListener getListSelectionListener(final JComponentAdapter list, final JButton nextButton, final JButton cancelButton) {
		queryFunctions.removeAllElements();
		queryFunctions.add("distinct-values");
		queryFilter = "/scenario/world/"+regionQueryPortion+"/";
		return (new ListSelectionListener() {
			public void valueChanged(ListSelectionEvent e) {
				int[] selectedInd = list.getSelectedRows();
				if(selectedInd.length == 0 && qg.currSel != 0) {
					nextButton.setEnabled(false);
					cancelButton.setText(" Cancel "/*cancelTitle*/);
					/*
				} else if(qg.currSel == 1 || qg.currSel == 2) {
					nextButton.setEnabled(true);
				} else if((qg.isSumable && (selectedInd[0] == 0 || selectedInd[0] == 1)) || selectedInd.length > 1
					|| ((String)list.getSelectedValues()[0]).startsWith("Group:")) {
					nextButton.setEnabled(false);
					cancelButton.setText("Finished");
				} else if(qg.currSel != 6 && !qg.isSumable) {
					nextButton.setEnabled(true);
					cancelButton.setText("Finished");
				} else if(qg.currSel != 6){
					nextButton.setEnabled(true);
					*/
				} else {
					cancelButton.setText("Finished");
				}
			}
		});
	}
	public void doFinish(JComponentAdapter list) {
		++qg.currSel;
		updateSelected(list);
		--qg.currSel;
		createXPath();
		queryFunctions = null;
		queryFilter = null;
	}
	public JComponentAdapter doBack(JComponentAdapter list, JLabel label) {
		// doing this stuff after currSel has changed now..
		// have to sub 1
		/*
		if(qg.currSel == 2) {
			sectorList = null;
		} else if(qg.currSel == 3) {
			subsectorList = null;
		} else if(qg.currSel == 4) {
			techList = null;
		} else if(qg.currSel == 5) {
			inputList = null;
		}
		*/
		return updateList(list, label);
	}
	public JComponentAdapter doNext(JComponentAdapter list, JLabel label) {
		// being moved to after currSel changed, adjust numbers
		updateSelected(list);
		if(qg.currSel == 3) {
			qg.var = "demand-currency";
			qg.isSumable = false;
		}
		return updateList(list, label);
	}
	public boolean isAtEnd() {
		return qg.currSel == 3-1;
	}
	public JComponentAdapter updateList(JComponentAdapter list, JLabel label) {
		/*
		Map temp = new HashMap();
		temp.put("Select This!!", false);
		switch(qg.currSel) {
			case 2: {
					list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
					label.setText("Select Stuff:");
					break;
			}
			*/
			/*
			case 3: {
					list.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
					if(sectorList == null) {
						sectorList = createList(sectorQueryPortion+"/@name", false);
						sectorList.putAll(createList(sectorQueryPortion+"/group/@name", true));
					}
					temp = sectorList;
					label.setText("Select Sector:");
					break;
			}
			case 4: {
					if(subsectorList == null) {
						subsectorList = createList(createListPath(4), false);
					}
					temp = subsectorList;
					label.setText("Select Subsector:");
					break;
			}
			case 5: {
					if(techList == null) {
						techList = createList(createListPath(5), false);
					}
					temp = techList;
					label.setText("Select Technology:");
					break;
			}
			case 6: {
					if(inputList == null) {
						inputList = createList(createListPath(6), false);
					}
					temp = inputList;
					label.setText("Select Input:");
					break;
			}
			*/
		/*
			default: System.out.println("Error currSel: "+qg.currSel);
		}
		Vector tempVector = new Vector();
		String[] currKeys = (String[])temp.keySet().toArray(new String[0]);
		((JList)list.getModel()).setListData(currKeys);
		// check the maps to see which ones are true and add it to the list of selected
		for (int i = 0; i < currKeys.length; ++i) {
			if (((Boolean)temp.get(currKeys[i])).booleanValue()) {
				tempVector.addElement(new Integer(i));
			}
		}
		int[] selected = new int[tempVector.size()];
		for (int i = 0; i < selected.length; i++) {
			selected[i] = ((Integer)tempVector.get(i)).intValue();
		}
		temp = null;
		tempVector = null;
		list.setSelectedRows(selected);
		*/
		return list;
	}
	public void updateSelected(JComponentAdapter list) {
		/*
		Object[] selectedKeys = list.getSelectedValues();
		Map selected = null;
		switch(qg.currSel -1) {
			case 1: {
					return;
			}
			case 2: {
					selected = varList;
					break;
			}
			case 3: {
					selected = sectorList;
					break;
			}
			case 4: {
					selected = subsectorList;
					break;
			}
			case 5: {
					selected = techList;
					break;
			}
			case 6: {
					selected = inputList;
					break;
			}
			default: System.out.println("Error currSel: "+qg.currSel);
		}
		for(Iterator it = selected.entrySet().iterator(); it.hasNext(); ) {
			((Map.Entry)it.next()).setValue(new Boolean(false));
		}
		for(int i = 0; i < selectedKeys.length; ++i) {
			selected.put(selectedKeys[i], new Boolean(true));
		}
		*/
	}
	public String createListPath(int level) {
		/*
		Map tempMap;
		StringBuffer ret = new StringBuffer();
		boolean added = false;
		boolean gq = false;
		if(level == -1) {
			gq = true;
			qg.sumAll = qg.group = false;
			level = 7;
		}
		for(int i = 0; i < level-3; ++i) {
			if(i == 0) {
				tempMap = sectorList;
				ret.append(sectorQueryPortion.substring(0, sectorQueryPortion.length()-1));
			} else if(i == 1){
				tempMap = subsectorList;
				ret.append(subsectorQueryPortion.substring(0, subsectorQueryPortion.length()-1));
			} else if(i == 2){
				tempMap = techList;
				ret.append(technologyQueryPortion.substring(0, technologyQueryPortion.length()-1));
				//++i;
			} else {
				tempMap = inputList;
				ret.append(inputQueryPortion.substring(0, inputQueryPortion.length()-1));
				++i;
			}
			added = false;
			if(tempMap == null) {
				ret.append("]/");
				continue;
			}
			if(qg.isSumable && ((Boolean)tempMap.get("Sum All")).booleanValue()) {
				qg.sumAll = true;
			}
			if(qg.isSumable && ((Boolean)tempMap.get("Group All")).booleanValue()) {
				qg.group = true;
			}
			//for(Iterator it = tempMap.entrySet().iterator(); it.hasNext() && !((Boolean)tempMap.get("Sum All")).booleanValue(); ) 
			for(Iterator it = tempMap.entrySet().iterator(); it.hasNext() && !(qg.sumAll || qg.group); ) {
				Map.Entry me = (Map.Entry)it.next();
				if(((Boolean)me.getValue()).booleanValue()) {
					if(!added) {
						ret.append(" and (");
						added = true;
					} else {
						ret.append(" or ");
					}
					if(!gq && ((String)me.getKey()).startsWith("Group:")) {
						ret.append(expandGroupName(((String)me.getKey()).substring(7)));
						gq = true;
					} else {
						ret.append("(@name='"+me.getKey()+"')");
					}
				}
			}
			if(added) {
				ret.append(" )]/");
			} else {
				ret.append("]/");
			}
		}
		if(level == 3) {
			ret.append(sectorQueryPortion+"/@name");
		} else if(level == 4) {
			ret.append(subsectorQueryPortion+"/@name");
		} else if(level == 5) {
			ret.append(technologyQueryPortion+"/@name");
		} else if(level == 6) {
			ret.append(inputQueryPortion+"/@name");
		} else {
			ret.append(qg.var).append("/node()");
			//ret += "period/"+var+"/node()";
			System.out.println("The xpath is: "+ret.toString());
		}
		if(gq) {
			qg.group = true;
			qg.sumAll = true;
		}
		return ret.toString();
		*/
		// do I need to do anything real?
		return qg.xPath;
	}
	/*
	private String expandGroupName(String gName) {
		String query;
		StringBuffer ret = new StringBuffer();
		if(qg.currSel == 3) {
			//query = "supplysector";
			query = sectorQueryPortion;
		} else if(qg.currSel == 4) {
			query = sectorQueryPortion+"/"+subsectorQueryPortion;
		} else if(qg.currSel == 5){
			query = sectorQueryPortion+"/"+subsectorQueryPortion+"/"+technologyQueryPortion;
		} else {
			query = sectorQueryPortion+"/"+subsectorQueryPortion+"/"+technologyQueryPortion+
				"/"+inputQueryPortion;
		}
		XmlResults res = XMLDB.getInstance().createQuery(query+"[child::group[@name='"+gName+"']]/@name", queryFilter, queryFunctions);
		try {
			while(res.hasNext()) {
				ret.append("(@name='").append(res.next().asString()).append("') or ");
			}
		} catch(XmlException e) {
			e.printStackTrace();
		}
		ret.delete(ret.length()-4, ret.length());
		XMLDB.getInstance().printLockStats("DemandComponentsQueryBuilder.expandGroupName");
		return ret.toString();
	}
	*/
	private void createXPath() {
		qg.axis1Name = "input";
		qg.nodeLevel = new DataPair<String, String>("input", "name");
		qg.axis2Name = "sector";
		qg.yearLevel = new DataPair<String, String>("sector", "name");
		qg.group = true;
		qg.var = "demand-currency";
		/*
		qg.xPath = "*[@type='sector' and @name='Trade']/"+subsectorQueryPortion+"/"+technologyQueryPortion+"/"+
			inputQueryPortion+"/demand-currency/text()";
			*/
		// TODO: rewrite this query so that it is cleaner and easier to understand what is going on.
		qg.xPath = sectorQueryPortion+"/"+subsectorQueryPortion+"/"+baseTechnologyQueryPortion+"/"
			+inputQueryPortion+"/demand-currency[(@year >= parent::*/parent::*/@year) and "
			+"(((parent::*/parent::*/@name = 'Trade' or parent::*/parent::*/@name = 'Government') and (@year = parent::*/parent::*/@year)) "
			+"or (parent::*/parent::*/@name != 'Trade' and parent::*/parent::*/@name != 'Government' and ((parent::*/@name != 'Capital') or "
			+"(parent::*/@name = 'Capital' and (local-name(parent::*/parent) != 'productionSector' "
			+"or (local-name(parent::*/parent) = 'productionSector' and @year = parent::*/parent::*/@year))))))]/text()";
		
		/* | "+sectorQueryPortion+"/"+subsectorQueryPortion+"/"+
			technologyQueryPortion+"/*[local-name() = 'output' or local-name() = 'annual-investment']/text() | "+
			"collection('database.dbxml')/scenario/world/Marketplace/market[child::MarketGoodOrFuel[ child::text() = /scenario/world/regionCGE/factorSupply/@name]]/supply/text()";
			*/
	}
	/*
	private Map createList(String path, boolean isGroupNames) {
		LinkedHashMap ret = new LinkedHashMap();
		if(!isGroupNames && qg.isSumable) {
			ret.put("Sum All", new Boolean(false));
			ret.put("Group All", new Boolean(false));
		}
		XmlResults res = XMLDB.getInstance().createQuery(path, queryFilter, queryFunctions);
		try {
			while(res.hasNext()) {
				if(!isGroupNames) {
					ret.put(res.next().asString(), new Boolean(false));
				} else { 
					ret.put("Group: "+res.next().asString(), new Boolean(false));
				}
			}
		} catch(XmlException e) {
			e.printStackTrace();
		}
		res.delete();
		XMLDB.getInstance().printLockStats("DemandComponentsQueryBuilder.createList");
		return ret;
	}
	*/
	public String getCompleteXPath(Object[] regions) {
		boolean added = false;
		StringBuffer ret = new StringBuffer();
		if(((String)regions[0]).equals("Global")) {
			ret.append(regionQueryPortion+"/");
			//regionSel = new int[0]; 
			regions = new Object[0];
		}
		for(int i = 0; i < regions.length; ++i) {
			if(!added) {
				ret.append(regionQueryPortion.substring(0, regionQueryPortion.length()-1)).append(" and (");
				added = true;
			} else {
				ret.append(" or ");
			}
			ret.append("(@name='").append(regions[i]).append("')");
		}
		if(added) {
			ret.append(" )]/");
		}
		int pipeIndex = qg.getXPath().indexOf('|');
		String part1 = qg.getXPath().substring(0, pipeIndex+1);
		String part2 = qg.getXPath().substring(pipeIndex+1, qg.getXPath().length());
		String retStr = ret.toString();
		//System.out.println("XPath would be: "+ret.append(part1).append(retStr).append(part2));
		return ret.append(qg.getXPath()).toString();
		/*
		return ret.append(part1)
			.append(" collection('database.dbxml')/scenario[ (@name='test' and @date='2006-27-9T22:20:46-00:00') ]/world/")
			.append(retStr).append(part2).toString();
			*/
	}
	public String getXMLName() {
		return xmlName;
	}
	public List<String> getDefaultCollpaseList() {
		return new Vector<String>();
	}
	public Map addToDataTree(XmlValue currNode, Map dataTree, DataPair<String, String> axisValue, boolean isGlobal) throws Exception {
		// stop point for recursion is when we reach the root
		if (currNode.getNodeType() == XmlValue.DOCUMENT_NODE) {
			return dataTree;
		}

		// recursively process parents first
		Map tempMap = addToDataTree(currNode.getParentNode(), dataTree, axisValue, isGlobal);

		final String nodeName = currNode.getNodeName();
		final Map<String, String> attrMap = XMLDB.getAttrMap(currNode);

		// attemp to find axis values at the current node
		String type = attrMap.get("type");
		boolean addedNodeLevel = false;
		boolean addedYearLevel = false;
		if(qg.nodeLevel.getKey().equals(type)) {
			addedNodeLevel = true;
			axisValue.setValue(attrMap.get(qg.nodeLevel.getValue()));
		} 
		if(qg.yearLevel.getKey().equals(type)) {
			addedYearLevel = true;
			String sectorType = attrMap.get(qg.yearLevel.getValue());
			if(sectorType.equals("Household")) {
				axisValue.setKey("Consumption");
			} else if(sectorType.equals("Government")) {
				axisValue.setKey("Government");
			} else if(sectorType.equals("Investment")) {
				axisValue.setKey("Investment");
			} else if(sectorType.equals("Trade")) {
				axisValue.setKey("Trade");
			} else {
				axisValue.setKey("Intermediate Production");
			}
		}
		if(type == null) {
			type = nodeName;
		}
		if(type.equals("demand-currency") || type.equals("region")) {
			String attr;
			if(type.equals("region")) {
				attr = "region@"+attrMap.get("name");
			} else {
				attr = "year@"+attrMap.get("year");
			}
			if(!tempMap.containsKey(attr)) {
				tempMap.put(attr, new HashMap());
			}
			tempMap = (Map)tempMap.get(attr);
		} 
		return tempMap;
	}
}