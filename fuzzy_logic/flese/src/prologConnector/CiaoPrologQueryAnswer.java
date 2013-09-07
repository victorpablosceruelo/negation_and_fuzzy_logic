package prologConnector;

import java.util.HashMap;

public class CiaoPrologQueryAnswer {

	private HashMap<String, CiaoPrologTermInJava> varsAnswers = null;
	public CiaoPrologQueryAnswer() {
		varsAnswers = new HashMap<String, CiaoPrologTermInJava>();
	}
	
	public void addCiaoPrologVariableAnswer(String variableName, CiaoPrologTermInJava variableAnswer) throws CiaoPrologQueryAnswerException {
		if (variableName == null) {
			throw new CiaoPrologQueryAnswerException("variableName cannot be null");
		}
		if ("".equals(variableName)) {
			throw new CiaoPrologQueryAnswerException("variableName cannot be empty string.");
		}
		if (variableAnswer == null) {
			throw new CiaoPrologQueryAnswerException("variableAnswer cannot be null");
		}
		
		this.varsAnswers.put(variableName, variableAnswer);
	}

	public CiaoPrologTermInJava getCiaoPrologQueryVariableAnswer(String variableName) throws CiaoPrologQueryAnswerException {
		if (variableName == null) {
			throw new CiaoPrologQueryAnswerException("variableName cannot be null");
		}
		if ("".equals(variableName)) {
			throw new CiaoPrologQueryAnswerException("variableName cannot be empty string.");
		}

		return this.varsAnswers.get(variableName);
	}
}
