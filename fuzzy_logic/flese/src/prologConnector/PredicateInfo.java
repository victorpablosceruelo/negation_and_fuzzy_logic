package prologConnector;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Set;

import prologConnector.moreInfo.PredMoreInfoAbstract;
import prologConnector.moreInfo.PredMoreInfoInterface;
import constants.KConstants;

public class PredicateInfo {

	private String predicateName = null;
	private String[][] predicateTypes = null;
	private int predicateArity = 0;
	private HashMap<String, PredMoreInfoInterface> predicateMoreInfo = new HashMap<String, PredMoreInfoInterface>();

	public PredicateInfo(CiaoPrologQueryAnswer answer) throws PredicateInfoException {

		if (answer == null)
			throw new PredicateInfoException("answer cannot be null");

		try {
			setPredicateName(answer);
			setPredicateTypes(answer);
			setPredicateArity(answer);
			setPredicateMoreInfo(answer);

		} catch (CiaoPrologConnectorException e) {
			e.printStackTrace();
			throw new PredicateInfoException("CiaoPrologConnectorException: " + e.getMessage());
		}
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	private void setPredicateName(CiaoPrologQueryAnswer answer) throws CiaoPrologConnectorException, PredicateInfoException {
		predicateName = answer.getCiaoPrologQueryVariableAnswer(KConstants.ProgramIntrospectionFields.predicateName).toString();
		if (predicateName == null) {
			throw new PredicateInfoException("predicateName cannot be null");
		}
		if ("".equals(predicateName)) {
			throw new PredicateInfoException("predicateName cannot be empty string");
		}
	}

	private void setPredicateTypes(CiaoPrologQueryAnswer answer) throws CiaoPrologConnectorException, PredicateInfoException {
		predicateTypes = null;
		CiaoPrologTermInJava term = answer.getCiaoPrologQueryVariableAnswer(KConstants.ProgramIntrospectionFields.predicateTypes);
		if (term == null) {
			throw new PredicateInfoException("predicateTypes cannot be null");
		}
		if (!term.isList()) {
			throw new PredicateInfoException("predInfoTypes is not a list.");
		}
		int typesListLength = term.length();
		predicateTypes = new String[typesListLength][];

		for (int i = 0; i < typesListLength; i++) {
			CiaoPrologTermInJava type = term.atPosition(i);
			if (type == null) {
				throw new PredicateInfoException("predicateTypes individual type cannot be null");
			}
			if (!type.isList()) {
				throw new PredicateInfoException("predInfoTypes individual type is not a list.");
			}
			int typeListLength = type.length();
			predicateTypes[i] = new String[typeListLength];
			for (int j = 0; j < typeListLength; j++) {
				predicateTypes[i][j] = type.atPosition(j).toString();
			}
		}

	}

	private void setPredicateArity(CiaoPrologQueryAnswer answer) throws PredicateInfoException, CiaoPrologConnectorException {
		String predicateArityString = answer.getCiaoPrologQueryVariableAnswer(KConstants.ProgramIntrospectionFields.predicateArity)
				.toString();
		if (predicateArityString == null) {
			throw new PredicateInfoException("predicateArity cannot be null");
		}
		if ("".equals(predicateArityString)) {
			throw new PredicateInfoException("predicateArity cannot be empty string");
		}

		predicateArity = Integer.parseInt(predicateArityString);
	}

	private void setPredicateMoreInfo(CiaoPrologQueryAnswer answer) throws PredicateInfoException, CiaoPrologConnectorException {
		
		CiaoPrologTermInJava term = answer.getCiaoPrologQueryVariableAnswer(KConstants.ProgramIntrospectionFields.predicateMoreInfo);
		predicateMoreInfo = PredMoreInfoAbstract.getHashMapWithMoreInfoObjects(term);
		
	}

	public PredMoreInfoInterface getPredicateMoreInfoAs(String type) {
		return predicateMoreInfo.get(type);
	}
	
	public Set<String> getPredicateMoreInfoKeys() {
		Set<String> keysSet = predicateMoreInfo.keySet();
		return keysSet;
	}
	
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public String getPredicateName() {
		return predicateName;
	}

	public String[][] getPredicateTypes() {
		return predicateTypes;
	}

	public int getPredicateArity() {
		return predicateArity;
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public boolean hasType(String[] type) {
		int i = 0;
		int typesLength = predicateTypes.length;
		boolean found = false;
		while (!found && i < typesLength) {
			if (predicateTypes[i].length == type.length) {
				boolean isThisOne = true;
				for (int j = 0; j < predicateTypes[i].length; j++) {
					if (!predicateTypes[i][j].equals(type[j])) {
						isThisOne = false;
					}
				}
				if (isThisOne) {
					found = true;
				}
			}
			i++;
		}
		return found;
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public String [][] getFullyDefinedTypes(String [] type) {
		ArrayList<String []> results = new ArrayList<String []>();

		for (int j = 0; j < predicateTypes.length; j++) {
			boolean valid = false;
			if (predicateTypes[j].length == type.length) {
				int length = predicateTypes[j].length;
				valid = true;

				// All the types I can compare must be equal to the ones I
				// have.
				for (int k = 0; k < length; k++) {
					if (type[k] != null) { // If null, do not compare.
						valid = valid && predicateTypes[j][k].equals(type[k]);
					}
				}
			}
			if (valid) {
				results.add(predicateTypes[j]);
			}
		}

		return results.toArray(new String [results.size()][]);
	}
	
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

}
