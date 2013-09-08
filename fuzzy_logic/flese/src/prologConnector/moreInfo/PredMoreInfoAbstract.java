package prologConnector.moreInfo;

import java.util.HashMap;

import prologConnector.CiaoPrologTermInJava;
import prologConnector.PredicateInfoException;

public abstract class PredMoreInfoAbstract implements PredMoreInfoInterface {

	private String type = null;

	protected PredMoreInfoAbstract() {
		type = null;
	}

	public String getType() {
		return type;
	}

	protected void setType(String type) {
		this.type = type;
	}
	
	public String[] generateVariablesNames(String initialPredicate) {
		return new String[0];
	}
	

	public static HashMap<String, PredMoreInfoInterface> getHashMapWithMoreInfoObjects(CiaoPrologTermInJava term)
			throws PredicateInfoException {

		if (term == null) {
			throw new PredicateInfoException("setPredicateMoreInfo: term cannot be null");
		}
		if (!term.isList()) {
			throw new PredicateInfoException("setPredicateMoreInfo: term is not a list.");
		}
		int predMoreInfoLength = term.length();
		HashMap<String, PredMoreInfoInterface> predicateMoreInfoHashMap = new HashMap<String, PredMoreInfoInterface>();

		for (int i = 0; i < predMoreInfoLength; i++) {
			CiaoPrologTermInJava predMoreInfoTerm = term.atPosition(i);
			if (predMoreInfoTerm == null) {
				throw new PredicateInfoException("setPredicateMoreInfo: predMoreInfo cannot be null");
			}
			if (!predMoreInfoTerm.isArray()) {
				throw new PredicateInfoException("setPredicateMoreInfo: predMoreInfo is not an array");
			}
			if (!(predMoreInfoTerm.length() == 2)) {
				throw new PredicateInfoException("setPredicateMoreInfo: predMoreInfo is not an array of length 2");
			}

			CiaoPrologTermInJava predMoreInfoType = predMoreInfoTerm.atPosition(0);
			CiaoPrologTermInJava predMoreInfoInfo = predMoreInfoTerm.atPosition(1);
			PredMoreInfoInterface predMoreInfo = getPredMoreInfoFor(predMoreInfoType, predMoreInfoInfo);
			predicateMoreInfoHashMap.put(predMoreInfo.getType(), predMoreInfo);
		}
		return predicateMoreInfoHashMap;
	}

	private static PredMoreInfoInterface getPredMoreInfoFor(CiaoPrologTermInJava predMoreInfoType, CiaoPrologTermInJava predMoreInfoInfo)
			throws PredicateInfoException {
		if (predMoreInfoType == null) {
			throw new PredicateInfoException("predMoreInfoType is null");
		}
		String type = predMoreInfoType.toString();

		if (type == null) {
			throw new PredicateInfoException("type is null");
		}
		if ("".equals(type)) {
			throw new PredicateInfoException("type is empty string");
		}

		PredMoreInfoInterface predMoreInfo = getPredMoreInfoObjectFor(type);
		if (predMoreInfo == null) {
			throw new PredicateInfoException("no class to store PredicateMoreInfo information");
		} else {
			predMoreInfo.setMoreInfo(type, predMoreInfoInfo);
		}
		return predMoreInfo;
	}

	private static PredMoreInfoInterface getPredMoreInfoObjectFor(String type) {
		PredMoreInfoInterface predMoreInfo = null;

		if (type.equals("fuzzy_rule")) {
			predMoreInfo = new PredMoreInfoFuzzyRule();
		}

		if (type.equals("database")) {
			predMoreInfo = new PredMoreInfoDatabase();
		}

		return predMoreInfo;
	}

}
