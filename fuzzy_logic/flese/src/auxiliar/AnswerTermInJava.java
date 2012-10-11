package auxiliar;

import org.apache.commons.logging.Log;

import CiaoJava.PLList;
import CiaoJava.PLTerm;
import CiaoJava.PLVariable;

public class AnswerTermInJava {

	private int listLength = 0;
	private String singleAnswerTerm = null;
	private AnswerTermInJava[] compositeAnswerTerm = null;
	
	private PLTerm prologQueryAnswer = null;
	private String creationMsgs = "";
	
	public AnswerTermInJava(PLTerm term, PLTerm prologQueryAnswer) throws Exception{
		
		if (term.isVariable()) {
			PLVariable variable = (PLVariable) term;
			PLTerm binding = variable.getBinding();
			if (binding == null) {
				creationMsgs += "variable has no binding. ";
				singleAnswerTerm = variable.toString();
			}
			else {
				creationMsgs += binding.toString();
				if (binding.isList()) {
					creationMsgs += " is a list. ";
					PLList list = (PLList) binding;
					listLength = list.length();
					if (listLength > 0) {
						compositeAnswerTerm = new AnswerTermInJava [listLength];
						for (int i=0; i<listLength; i++) {
							compositeAnswerTerm[i] = new AnswerTermInJava(list.getHead(), prologQueryAnswer);
							list = (PLList) list.getTail();
						}
					}
				}
				else {
					creationMsgs += " is not a list. ";
					singleAnswerVar = binding.toString();
				}
			}
		}
		else {
			term.toString();
		}
	}
	
	public String toString = null;
}
