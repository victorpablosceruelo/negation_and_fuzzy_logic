package prologConnector;

import java.util.ArrayList;

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
}
