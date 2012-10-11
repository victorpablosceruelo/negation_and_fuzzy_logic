package auxiliar;

import CiaoJava.PLAtom;
import CiaoJava.PLFloat;
import CiaoJava.PLInteger;
import CiaoJava.PLList;
import CiaoJava.PLString;
import CiaoJava.PLStructure;
import CiaoJava.PLTerm;
import CiaoJava.PLVariable;

public class AnswerTermInJava {

	private int listLength = 0;
	private String singleAnswerTerm = null;
	private AnswerTermInJava[] compositeAnswerTerm = null;
	
	private PLTerm prologQueryAnswer = null;
	private String creationMsgs = "";
	
	public AnswerTermInJava(PLTerm term, PLTerm prologQueryAnswer) {
		creationMsgs += " ";
		conversion(term, prologQueryAnswer);
	}
	
	private void conversion(PLTerm term, PLTerm prologQueryAnswer) {
		// Log ...
		creationMsgs += "\nTerm: " + term.toString() + " ";
		// For variables.
		if (term.isVariable()) {
			PLVariable prologVariable = (PLVariable) term;
			creationMsgs += "is a variable ";
			PLTerm binding = prologVariable.getBinding();
			if (binding == null) {
				creationMsgs += "without binding. ";
				singleAnswerTerm = "-variable-";
			}
			else {
				creationMsgs += "with binding: "+binding.toString();
				conversion(binding, prologQueryAnswer);
			}
		}
		// For lists.
		if (term.isList()) {
			creationMsgs += "is a list. ";
			PLList prologList = (PLList) term;
			listLength = prologList.length();
			if (listLength > 0) {
				compositeAnswerTerm = new AnswerTermInJava [listLength];
				for (int i=0; i<listLength; i++) {
					compositeAnswerTerm[i] = new AnswerTermInJava(prologList.getHead(), prologQueryAnswer);
					creationMsgs += compositeAnswerTerm[i].getCreationMsgs();
					if ((prologList.getTail() != null) && (prologList.getTail().isList())) {
						prologList = (PLList) prologList.getTail();
					}
					else creationMsgs += "\nRemaining list: " + prologList.getTail().toString() + " ";
				}
			}
		}
		// For integer
		if (term.isInteger()) {
			creationMsgs += " is an integer. ";
			PLInteger prologInteger = (PLInteger) term;
			singleAnswerTerm = prologInteger.toString();
		}
		// For float
		if (term.isFloat()) {
			creationMsgs += " is a float. ";
			PLFloat prologInteger = (PLFloat) term;
			singleAnswerTerm = prologInteger.toString();
		}
		// For atom ???
		if (term.isAtom()) {
			creationMsgs += " is an atom. ";
			PLAtom prologInteger = (PLAtom) term;
			singleAnswerTerm = prologInteger.toString();
		}
		
		// For structure
		if (term.isStructure()) {
			creationMsgs += " is an structure. ";
			PLStructure prologInteger = (PLStructure) term;
			singleAnswerTerm = prologInteger.toString();			
		}		
		// For string
		if (term.isString()) {
			creationMsgs += " is an string. ";
			PLString prologInteger = (PLString) term;
			singleAnswerTerm = prologInteger.toString();			
		}
		
		if (! (term.isVariable() || term.isList() || term.isInteger() || term.isStructure() || 
				term.isString() || term.isAtom() || term.isFloat())) {
			creationMsgs += " -- what the hell is this??? ";
			singleAnswerTerm = term.toString();
		}
	}
	
	public String toString () {
		String retVal = " ";
		if (singleAnswerTerm != null) retVal += singleAnswerTerm;
		if (compositeAnswerTerm != null) {
			retVal += "(";
			for (int i=0; i<listLength; i++) {
				retVal += compositeAnswerTerm[i].toString();
				if ((i+1) < listLength) retVal += ", ";
			}
			retVal += ")";
		}
		retVal += " ";
		return retVal;
	}
	
	public PLTerm getprologQueryAnswer() {
		return prologQueryAnswer;
	}
	public String getCreationMsgs () {
		return creationMsgs;
	}
}



////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////
