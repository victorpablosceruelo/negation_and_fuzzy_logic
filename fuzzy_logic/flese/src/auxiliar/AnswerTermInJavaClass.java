package auxiliar;

import CiaoJava.PLAtom;
import CiaoJava.PLFloat;
import CiaoJava.PLInteger;
import CiaoJava.PLList;
import CiaoJava.PLString;
import CiaoJava.PLStructure;
import CiaoJava.PLTerm;
import CiaoJava.PLVariable;

public class AnswerTermInJavaClass {

	private String singleAnswerTerm = null;
	private AnswerTermInJavaClass[] compositeAnswerTerm = null;
	
	private PLTerm prologQueryAnswer = null;
	private String creationMsgs = "";
	
	public AnswerTermInJavaClass(PLTerm term, PLTerm prologQueryAnswer) throws AnswerTermInJavaClassException {
		creationMsgs += " ";
		conversion(term, prologQueryAnswer);
	}
	
	private void conversion(PLTerm term, PLTerm prologQueryAnswer) throws AnswerTermInJavaClassException {
		if (term == null) throw new AnswerTermInJavaClassException("term is null.");
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
			int listLength = prologList.length();
			if (listLength > 0) {
				compositeAnswerTerm = new AnswerTermInJavaClass [listLength];
				for (int i=0; i<listLength; i++) {
					compositeAnswerTerm[i] = new AnswerTermInJavaClass(prologList.getHead(), prologQueryAnswer);
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
			PLFloat prologFloat = (PLFloat) term;
			singleAnswerTerm = prologFloat.toString();
		}
		// For atom ???
		if (term.isAtom()) {
			creationMsgs += " is an atom. ";
			PLAtom prologAtom = (PLAtom) term;
			singleAnswerTerm = prologAtom.toString();
		}
		
		// For structure
		if (term.isStructure()) {
			creationMsgs += " is an structure. ";
			PLStructure prologStructure = (PLStructure) term;
			String functor = prologStructure.getFunctor();
			if (! (",".equals(functor))) {
				singleAnswerTerm = functor;
			}
			
			int listLength = prologStructure.getArity();
			if (listLength > 0) {
				compositeAnswerTerm = new AnswerTermInJavaClass [listLength];

				for (int i=0; i<listLength; i++) {
					compositeAnswerTerm[i] = new AnswerTermInJavaClass(prologStructure.getArg(i), prologQueryAnswer);
					creationMsgs += compositeAnswerTerm[i].getCreationMsgs();
				}
			}
			// singleAnswerTerm = prologStructure.toString();			
		}		
		// For string
		if (term.isString()) {
			creationMsgs += " is an string. ";
			PLString prologString = (PLString) term;
			singleAnswerTerm = prologString.toString();			
		}
		
		if (! (term.isVariable() || term.isList() || term.isInteger() || term.isStructure() || 
				term.isString() || term.isAtom() || term.isFloat())) {
			creationMsgs += " -- what the hell is this??? ";
			singleAnswerTerm = term.toString();
		}
	}
	
	public String toString () {
		String retVal = "";
		if ((singleAnswerTerm != null) && (! ",".equals(singleAnswerTerm))) retVal += singleAnswerTerm;
		if (compositeAnswerTerm != null) {
			retVal += "(";
			for (int i=0; i<compositeAnswerTerm.length; i++) {
				retVal += compositeAnswerTerm[i].toString();
				if ((i+1) < compositeAnswerTerm.length) retVal += ", ";
			}
			retVal += ")";
		}
		retVal += "";
		return retVal;
	}
	
	public PLTerm getprologQueryAnswer() {
		return prologQueryAnswer;
	}
	public String getCreationMsgs () {
		return creationMsgs;
	}
	
	public boolean isList() {
		return ((singleAnswerTerm == null) && (compositeAnswerTerm != null));
	}
	public int length() {
		if (! isList()) return 0;
		else
			return compositeAnswerTerm.length;
	}
	public AnswerTermInJavaClass atPosition(int i) {
		if (isList() && (i > 0) && (i < length())) {
			return compositeAnswerTerm[i];
		}
		else return null;
	}
	public String toJavaScript(boolean withCommasAround) {
		String retVal="";
		boolean subTermWithCommasAround = false;
		if ((compositeAnswerTerm == null) && (singleAnswerTerm == null)) { 
			withCommasAround=false;
			retVal="null";
		}
		
		// An atom or a structure.
		if (singleAnswerTerm != null) {
			if (withCommasAround) retVal += "\'";
			retVal += singleAnswerTerm;
		}
		// A list or a structure.
		if (compositeAnswerTerm != null) {
			if (singleAnswerTerm == null) {
				retVal+="new Array("; // A list
				subTermWithCommasAround = true;
			}
			else retVal+="("; // A structure
			
			for (int i=0; i<compositeAnswerTerm.length; i++) {
				retVal+=compositeAnswerTerm[i].toJavaScript(subTermWithCommasAround);
				if (i+1 < compositeAnswerTerm.length) retVal+=",";
			}
			retVal += ")";
		}
		if ((singleAnswerTerm != null) && (withCommasAround)) retVal += "\'";
		return retVal;
	}
}



////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////
