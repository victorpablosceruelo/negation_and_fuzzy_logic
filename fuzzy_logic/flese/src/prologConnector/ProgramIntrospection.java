package prologConnector;

import java.util.ArrayList;
import java.util.Set;

public class ProgramIntrospection {

	ArrayList<PredicateInfo> predicatesInfos = null;

	public ProgramIntrospection() {
		predicatesInfos = new ArrayList<PredicateInfo>();
		resetComputedInformation();
	}

	/**
	 * This method resets all computed information.
	 */
	private void resetComputedInformation() {

	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public void addAnswerInfo(CiaoPrologQueryAnswer answer) {
		resetComputedInformation();

		PredicateInfo predicateInfo;
		try {
			predicateInfo = new PredicateInfo(answer);
			predicatesInfos.add(predicateInfo);
		} catch (PredicateInfoException e) {
			e.printStackTrace();
		}
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public PredicateInfo getPredicateInfo(String predicateName) throws CiaoPrologConnectorException {
		if (predicateName == null) {
			throw new CiaoPrologConnectorException("predicateName cannot be null.");
		}
		if ("".equals(predicateName)) {
			throw new CiaoPrologConnectorException("predicateName cannot be empty string.");
		}

		PredicateInfo predicateInfo = null;
		int i = 0;
		boolean found = false;
		while (i < predicatesInfos.size() && (!found)) {
			predicateInfo = predicatesInfos.get(i);
			if (predicateName.equals(predicateInfo.getPredicateName()))
				found = true;
			else
				i++;
		}
		if (predicateInfo == null) {
			throw new CiaoPrologConnectorException("returned answer cannot be null.");
		}
		return predicateInfo;
	}
	
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public PredicateInfo [] getPredicatesInfosByMoreInfoKey(String key) throws CiaoPrologConnectorException {
		if (key == null) {
			throw new CiaoPrologConnectorException("key cannot be null.");
		}
		if ("".equals(key)) {
			throw new CiaoPrologConnectorException("key cannot be empty string.");
		}
		ArrayList<PredicateInfo> predicatesArrayList = new ArrayList<PredicateInfo>();
		PredicateInfo predicateInfo = null;
		Set<String> keys = null;
		int i = 0;
		boolean valid = false;
		
		while (i < predicatesInfos.size()) {
			predicateInfo = predicatesInfos.get(i);
			keys = predicateInfo.getPredicateMoreInfoKeys();
			valid = keys.contains(key); 
			if (valid) {
				predicatesArrayList.add(predicateInfo);
			}
				i++;
		}
		
		return predicatesArrayList.toArray(new PredicateInfo[predicatesArrayList.size()]);
	}
	
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

}
