package auxiliar;

import org.apache.commons.logging.Log;

import CiaoJava.PLList;
import CiaoJava.PLTerm;
import CiaoJava.PLVariable;

public class AnswerConversionCiaoPrologToJavaClass {
	
	private Log LOG = null;
	private String creationMsgs = null;
	
	public AnswerConversionCiaoPrologToJavaClass(String preMsg, Log LOG) throws Exception {
		creationMsgs = preMsg;
		this.LOG = LOG;
	}
	
	public AnswerVarInJava convertAnswerToJava(, ) {
		creationMsgs += ("\n answer: " + prologQueryAnswer.toString());
		PLVariablesArrayToCreationMsgString(variables);
		
		AnswerVarInJava answer = new AnswerVarInJava(AnswerVarInJava.List, variables.length, prologQueryAnswer);
		
		for (int i=0; i < variables.length; i++) {
			// preMsg += variables[i].getBinding().toString();
			answer[i] = new AnswerVarInJava(i, variables[i], LOG);
			creationMsgs += answer[i].getCreationMsgs();
		}
		LOG.info("performQueryAux: " + preMsg + " ");
		
	}
	
	private PLVariable variableCopy;
	
	
	private String variableInText = null;
	private String [] variableListInText = null;
	
	public AnswerVarInJava(int index, PLVariable variable, Log LOG) {
	
		this.variableCopy = (PLVariable) variable.copy();
		this.LOG = LOG;
		
		
		creationMsgs += ("\n   var["+index+"]: ");
		bindingToString(binding);

	}
		
	private void bindingToString(PLTerm binding) {
		
	}
	
	/**
	 * Converts a Prolog PLList to an array of strings.
	 * @param list is the PLList
	 */
	private void listToString(PLList list) {
		if (list.isList()) {
			
		}
	}
	
	
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////


	
	public String getCreationMsgs() {
		return creationMsgs;
	}
}
