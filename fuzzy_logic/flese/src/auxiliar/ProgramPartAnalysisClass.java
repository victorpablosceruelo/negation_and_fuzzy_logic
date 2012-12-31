package auxiliar;

import java.util.ArrayList;
import java.util.Iterator;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class ProgramPartAnalysisClass {
	final Log LOG = LogFactory.getLog(ProgramPartAnalysisClass.class);
	
	private ArrayList <String> programSubPartLines = null;
	private ArrayList <String> programSubPartComments = null;
	
	private String programSubPartJoinedLine = null;
	private String head = null;
	private String predDefined = null;
	private String predNecessary = null;
	private String body = null;
	private String fuzzyBody = null;
	private int value = -1;
	private ArrayList <FunctionPoint> functionPoints = null;
	private String ruleBody = null;
	private String ruleAggregator = null;
	private int defaults_to = -1;
	private String if_condition = null;
	private String with_credibility = null;
	private String only_for_user = null;
	private boolean partIsIncomplete = false;
	
	private static final String whiteSpacesPattern = "[\\s]*";
	// private static final String anythingPattern    = "[\\s\\S]*";
	private static final String termNamePattern    = "[0-9a-zA-Z_-]+";
	private static final String predicatePattern   = whiteSpacesPattern + termNamePattern + "[\\(]{1}" + whiteSpacesPattern + 
													termNamePattern + whiteSpacesPattern + "[\\)]{1}" + whiteSpacesPattern;

	
	ProgramPartAnalysisClass() {
		this.programSubPartJoinedLine = "";
		this.programSubPartLines = new ArrayList <String>();
		this.programSubPartComments = new ArrayList <String>();
	}
	
	public String parse(String programLine) throws Exception {
		
		if (programLine == null) throw new Exception("program line is null");
		String programSubPartLine = null;

		// If there is a comment, save it.
		int index = programLine.indexOf("%");
		if (index > -1) {
			programSubPartComments.add(programLine.substring(index));
			programLine = programLine.substring(0, index);
		}
		else programSubPartComments.add(null);
		
		// If the line contains more than 1 clause, split it.
		index = programLine.indexOf(".");
		if (index > -1) {
			// It contains at least 1 clause.
			programSubPartLine = programLine.substring(0, index);
			programLine = programLine.substring(index);
			partIsIncomplete = false;
		}
		else {
			// It does not contain a clause.
			programSubPartLine = programLine.substring(0);
			programLine = "";
			partIsIncomplete = true;
		}
		
		programSubPartLines.add(programSubPartLine);
		programSubPartJoinedLine += programSubPartLine;
		
		if (! partIsIncomplete) {	
			index = programSubPartJoinedLine.indexOf(":-");
			if (index > -1) {
				head = removeSpacesBeforeAndAfter(programLine.substring(0, index));
				body = removeSpacesBeforeAndAfter(programLine.substring(0, index));
				programSubPartJoinedLine = "";
			}

			index = programSubPartJoinedLine.indexOf(":~");
			if (index > -1) {
				head = removeSpacesBeforeAndAfter(programLine.substring(0, index));
				fuzzyBody = removeSpacesBeforeAndAfter(programLine.substring(0, index));

				parseHead();
				parseFuzzyBody();
				programSubPartJoinedLine = "";
			}

			if (! ("".equals(programSubPartJoinedLine))) {
				head = removeSpacesBeforeAndAfter(programSubPartJoinedLine);
			}

			programLine = removeSpacesBeforeAndAfter(programLine);
			if ("".equals(programLine)) programLine = null;
		}
		return programLine;
	}
	
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////

	private String removeSpacesBeforeAndAfter(String input) {
		input = removeSpacesBefore(input);
		input = removeSpacesAfter(input);
		return input;
	}
	
	private String removeSpacesBefore(String input) {
		if (input == null) return input;
		while ((! "".equals(input)) && (input.charAt(0) == ' ')) {
			input = input.substring(1);
		}
		return input;
	}
	
	private String removeSpacesAfter(String input) {
		if (input == null) return input;
		while ((! "".equals(input)) && (input.charAt(input.length() -1) == ' ')) {
			input = input.substring(input.length() -1);
		}
		return input;
	}	
	
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////

	/*
	if (programPart.getFunctionAnalized() != null) {
		if ((localUserName.equals(fileOwner)) ||
			((programPart.getFunctionAnalized().getPredOwner() != null) && 
			 ((programPart.getFunctionAnalized().getPredOwner().equals(localUserName)) ||
			  (programPart.getFunctionAnalized().getPredOwner().equals(DEFAULT_DEFINITION))))) {
			addToFunctionsOrderedList(programPart.getFunctionAnalized());
		}
	}
	*/
	
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public String [] getLines () {
		String [] lines = new String[programSubPartLines.size()];
		for (int i=0; i<programSubPartLines.size(); i++) {
			lines[i] = programSubPartLines.get(i) + " " + programSubPartComments.get(i);
		}
		return lines;
	}
	
	public boolean partIsIncomplete () {
		return partIsIncomplete;
	}
	
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////

	private void parseHead() throws Exception {
		if (head == null) throw new Exception("head is null.");
		
		if (! head.matches(predicatePattern)) {
			LOG.info("Head does not match predicate pattern. Head: " + head);
			predDefined = null;
			predNecessary = null;
		}

		predDefined       = head.replaceAll(predicatePattern, "$1");
		predNecessary     = head.replaceAll(predicatePattern, "$2");
		// String predOthers = head.replaceAll(predicatePattern, "$3");

	}

	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////

	private void parseFuzzyBody() throws Exception {
		if (fuzzyBody == null) throw new Exception("fuzzy body is null.");
		
		fuzzyBody = removeSpacesBeforeAndAfter(fuzzyBody);
		
		int indexStart = fuzzyBody.indexOf("(");
		int indexMiddle = -1; 
		int indexEnd = -1;
		boolean error = false;
		
		if (indexStart == -1) throw new Exception("fuzzy body contains no functionality.");
		
		String functionality = removeSpacesBeforeAndAfter(fuzzyBody.substring(0, indexStart -1));
		String arguments = removeSpacesBeforeAndAfter(fuzzyBody.substring(indexStart +1)); 
		
		// For values.
		if ("value".equals(functionality)) {
			indexEnd = arguments.indexOf(")");
			if (indexEnd > -1) {
				try {
					value = Integer.parseInt(arguments.substring(0, indexEnd -1));
				}
				catch (Exception e) {
					error = true;
				}
				arguments = arguments.substring(indexEnd +1);
			}
		}
		
		// For functions.
		if ("function".equals(functionality)) {
			boolean morePoints = true;
			while (morePoints) {
				indexStart = 0;
				indexEnd = arguments.indexOf(")");
				if (indexEnd != -1) {

					indexStart = (arguments.substring(0, indexEnd -1)).indexOf("(");
					if (indexStart > -1) {
						indexMiddle = (arguments.substring(0, indexEnd -1)).indexOf(",", indexStart);
						if (indexMiddle > -1) {
							String pointX = arguments.substring(indexStart +1, indexMiddle -1);
							String pointY = arguments.substring(indexMiddle +1, indexEnd -1);
							
							if (functionPoints == null) {
								functionPoints = new ArrayList<FunctionPoint>();
							}
							functionPoints.add(new FunctionPoint(pointX, pointY));
						}
						else {
							// The format of a point is not correct. Omit it.
							morePoints = true;
							error = true;
						}
					}
					else {
						// Reached the end in the points sequence.
						morePoints = false;
					}
					// Remove the part that has been processed.
					arguments = arguments.substring(indexEnd +1);
				}
				else {
					// No end in the points sequence.
					morePoints = false;
					error = true;
				}
			}
		}

		// For rules.
		if ("rule".equals(functionality)) {
			ruleBody = "";
			indexEnd = arguments.indexOf(",");
			ruleAggregator = arguments.substring(0, indexEnd);
			arguments = arguments.substring(indexEnd+1);
			
			boolean moreBody = true;
			while (moreBody) {
				indexStart = 0;
				indexEnd = arguments.indexOf(")");
				if (indexEnd != -1) {
					indexStart = (arguments.substring(0, indexEnd -1)).indexOf("(");
					if (indexStart != -1) {
						ruleBody += arguments.substring(indexStart, indexEnd);
						arguments = arguments.substring(indexEnd +1);
					}
					else {
						moreBody = false;
						arguments = arguments.substring(indexEnd +1);
					}
				}
				else moreBody = false;
			}
		}

		// For defaults.
		if ("defaults_to".equals(functionality)) {
			indexEnd = arguments.indexOf(")");
			if (indexEnd > -1) {				
				defaults_to = Integer.parseInt(arguments.substring(0, indexEnd -1));
				arguments = arguments.substring(indexEnd +1);
			}
		}

		// Now we go for the condition, the credibility and the username filters.
		// Pred_Functor :~ P_B if Pred_Condition with_credibility (Op, Cred) only_for_user UserName
		String extraOption = null;
		
		
		indexStart = arguments.indexOf("(");
		while (indexStart > -1) {
			
			extraOption = removeSpacesBeforeAndAfter(arguments.substring(0, indexStart -1));
			arguments = arguments.substring(indexStart +1);			
			arguments = saveExtraOption(extraOption, arguments);

			// Go for the next filter.
			indexStart = arguments.indexOf("(");
		}
		
		if (error) throw new Exception("Error reading file.");
	}
	
	private String saveExtraOption(String extraOption, String arguments) {
		if ("if".equals(extraOption)) if_condition = "";
		if ("with_credibility".equals(extraOption)) with_credibility = "";
		if ("only_for_user".equals(extraOption)) only_for_user = "";
		
		boolean optionRead = false;
		int indexMiddle = 0, indexEnd = 0;
		
		while (! optionRead) {
			indexEnd = arguments.indexOf(")");
			if (indexEnd > -1) {
				indexMiddle = (arguments.substring(0, indexEnd)).indexOf("(");
				if (indexMiddle > -1) {
					if ("if".equals(extraOption)) if_condition += arguments.substring(indexMiddle, indexEnd);
					if ("with_credibility".equals(extraOption)) with_credibility += arguments.substring(indexMiddle, indexEnd);
					if ("only_for_user".equals(extraOption)) only_for_user += arguments.substring(indexMiddle, indexEnd);
				}
				else {
					optionRead = true;
				}
			}
			else return "";
		}
		return arguments.substring(indexEnd +1);
	}
	
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public void setPredDefined(String predDefined) {
		this.predDefined = predDefined;
	}
	
	public String getPredDefined() {
		return predDefined;
	}

	public void setPredNecessary(String predNecessary) {
		this.predNecessary = predNecessary;
	}
	
	public String getPredNecessary() {
		return predNecessary;
	}
	
	public void setPredOwner(String predOwner) {
		if ((predOwner != null) && (! "default definition".equals(predOwner)) && (! "".equals(predOwner))) {
			only_for_user = predOwner;
		}
	}
	
	public String getPredOwner() {
		if ((only_for_user == null) || (only_for_user != null) && ("".equals(only_for_user))) return "default definition";
		else return only_for_user;
	}

	public String getBody() {
		return body;
	}
	
	public int getValue() {
		return value;
	}
	
	public String getRuleBody() {
		return ruleBody;
	}
	
	public String getRuleAggregator() {
		return ruleAggregator;
	}

	public int getDefaultsTo() {
		return defaults_to;
	}
	
	public String getIfCondition() {
		return if_condition;
	}
	
	public String getWithCredibility() {
		return with_credibility;
	}
	
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public boolean isFunction() {
		return (functionPoints != null);
	}
	
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public String getFunctionInJavaScript() throws Exception {
		if (functionPoints == null) throw new Exception("functionPoints is null.");
		
		Iterator <FunctionPoint> iterator = functionPoints.iterator();
		FunctionPoint element = null;
		String result = "";
		
		if (functionPoints.size() > 0) {
			result += "new Array(";
		}
		
		if (iterator != null) {
			while (iterator.hasNext()) {
				element = iterator.next();
				result += element.toJavaScript();
				
				if (iterator.hasNext()) result += ", ";
			}
		}
		
		if (functionPoints.size() > 0) {
			result += ")";
		}
		
		return result;
	}
	
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public void updateFunction(String [] [] params) throws Exception {
		
		functionPoints = new ArrayList<FunctionPoint>();
		
		for (int i=0; i<params.length; i++) {
			functionPoints.add(new FunctionPoint(params[i][0], params[i][1]));
		}
	}
	
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////

}
