package auxiliar;

import java.util.ArrayList;
import java.util.Iterator;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class ProgramPartAnalysisClass {
	final Log LOG = LogFactory.getLog(ProgramPartAnalysisClass.class);
	public final static String DEFAULT_DEFINITION = "default definition";
	
	private ArrayList <String> programSubPartLines = null;
	private ArrayList <String> programSubPartComments = null;
	
	private String programSubPartLineAcc = null;
	private String head = null;
	private String predDefined = null;
	private String predNecessary = null;
	private String body = null;
	private String fuzzyBody = null;
	private float value = -1;
	private ArrayList <FunctionPoint> functionPoints = null;
	private String ruleBody = null;
	private String ruleAggregator = null;
	private float defaults_to = -1;
	private String if_condition = null;
	private String with_credibility = null;
	private String only_for_user = null;
	private boolean partIsIncomplete = false;
	
	private static final String whiteSpacesPattern = "[\\s]*";
	// private static final String anythingPattern    = "[\\s\\S]*";
	private static final String termNamePattern    = "([0-9a-zA-Z_-]+){1}";
	private static final String predicatePattern   = whiteSpacesPattern + termNamePattern + "[\\(]{1}" + whiteSpacesPattern + 
													termNamePattern + whiteSpacesPattern + "[\\)]{1}" + whiteSpacesPattern;

	
	ProgramPartAnalysisClass() {
		this.programSubPartLineAcc = "";
		this.programSubPartLines = new ArrayList <String>();
		this.programSubPartComments = new ArrayList <String>();
	}
	
	public String parse(String programLineIn) throws Exception {
		LOG.info("programLineIn: " + programLineIn);
		if (programLineIn == null) throw new Exception("program line is null");
		String programSubPartLine = null;

		// If there is a comment, save it.
		int index = programLineIn.indexOf("%");
		if (index > -1) {
			programSubPartComments.add(programLineIn.substring(index));
			programLineIn = removeSpacesBeforeAndAfter(programLineIn.substring(0, index));
		}
		else programSubPartComments.add(null);
		
		// If the line contains more than 1 clause, split it.
		index = programLineIn.indexOf(".");
		if (index +1 != programLineIn.length()) {
			while ((index > 0) && (! dotDenotesClauseEnd(index, programLineIn))) {
				index = programLineIn.indexOf(".", index +1);
			}
		}

		if (index > -1) { // We have 1 end of clause delimiter.
			// It contains at least 1 clause.
			programSubPartLine = programLineIn.substring(0, index);
			if ((index +1) < programLineIn.length()) programLineIn = programLineIn.substring(index +1);
			else programLineIn = "";
			partIsIncomplete = false;
		}
		else {
			if ("".equals(programSubPartLineAcc)) { // It is the beggining of a line.
				if ("".equals(programLineIn)) { // The line contains only a comment.
					partIsIncomplete = false;
				}
				else { // The line contains something but it is incomplete.
					partIsIncomplete = true;
				}
			}
			else { // We are inside a splitted subPart.
				partIsIncomplete = true;
				
			}
			// The subpart line is the full line (without comment).
			programSubPartLine = programLineIn;
			programLineIn = "";
		}
		
		programSubPartLines.add(programSubPartLine);
		programSubPartLineAcc += programSubPartLine;
		
		head = null;
		if (! partIsIncomplete) {	
			String msg = "partIsIncomplete: " + partIsIncomplete + "\n";
			msg += "programSubPartLine: " + programSubPartLine + "\n";
			msg += "programLineIn: " + programLineIn;
			
			index = programSubPartLineAcc.indexOf(":-");
			if (index > -1) {
				head = removeSpacesBeforeAndAfter(programSubPartLineAcc.substring(0, index));
				body = removeSpacesBeforeAndAfter(programSubPartLineAcc.substring(index +2));
				programSubPartLineAcc = "";
			}

			index = programSubPartLineAcc.indexOf(":~");
			if (index > -1) {
				head = removeSpacesBeforeAndAfter(programSubPartLineAcc.substring(0, index));
				fuzzyBody = removeSpacesBeforeAndAfter(programSubPartLineAcc.substring(index +2));

				parseHead();
				parseFuzzyBody();
				programSubPartLineAcc = "";
			}

			if ((head != null) && (! ("".equals(programSubPartLineAcc)))) {
				head = removeSpacesBeforeAndAfter(programSubPartLineAcc);
			}
			
			msg += "head: " + head + "\n";
			msg += "body: " + body + "\n";
			msg += "fuzzyBody: " + fuzzyBody + "\n";
			LOG.info(msg);
		}
		
		// Avoid problems when the line is formed by just white spaces.
		programLineIn = removeSpacesBeforeAndAfter(programLineIn);
		if ("".equals(programLineIn)) programLineIn = null;
		
		return programLineIn;
	}
	
	private boolean dotDenotesClauseEnd(int index, String programLineIn) throws Exception {
		if (index < 0) throw new Exception("Index out of range. index: " + index + " programLineIn: " + programLineIn);
		if (! (index < programLineIn.length())) throw new Exception("Index out of range. index: " + index + " programLineIn: " + programLineIn);
		if (programLineIn.charAt(index) != '.') throw new Exception("Character at index is not a dot. index: " + index + " programLineIn: " + programLineIn);
		
		int subStringBegins = index -1;
		while ( (subStringBegins >= 0) && 
				(! isDelimiter(programLineIn.charAt(subStringBegins)))) {
			subStringBegins--;
		}
		if (subStringBegins <= 0) subStringBegins = 0;
		
		int subStringEnds = index +1;
		while ( (subStringEnds < programLineIn.length()) && 
				(! isDelimiter(programLineIn.charAt(subStringEnds)))) {
			subStringEnds++;
		}
		if (subStringEnds >= programLineIn.length()) subStringEnds = programLineIn.length();
		
		String subString = programLineIn.substring(subStringBegins, subStringEnds);
		if (subString.length() < 3) return true;
		else {
			try {
				Float number = Float.parseFloat(subString);
				LOG.info("It is not a dot because it is a number. Number: " + number);
				return false;
			} catch (Exception e) {
				return true;
			}
		}
		
	}
	
	private boolean isDelimiter(char character) {
		if (character == ' ') return true;
		if (character == ',') return true;
		if (character == '.') return true;
		if (character == ')') return true;
		if (character == '(') return true;
		if (character == ']') return true;
		if (character == '[') return true;
		return false;
	}
	
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////

	private String removeSpacesBeforeAndAfter(String input) {
		input = removeSpacesBefore(input);
		input = removeSpacesAfter(input);
		// LOG.info("input: " + input);
		return input;
	}
	
	private String removeSpacesBefore(String input) {
		if (input == null) return input;
		while ((! "".equals(input)) && (input.charAt(0) == ' ')) {
			input = input.substring(1);
		}
		// LOG.info("input: " + input);
		return input;
	}
	
	private String removeSpacesAfter(String input) {
		if (input == null) return input;
		while ((! "".equals(input)) && (input.charAt(input.length() -1) == ' ')) {
			input = input.substring(0, input.length() -1);
		}
		// LOG.info("input: " + input);
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
		LOG.info("parseHead. head: " + head);
		if (head == null) throw new Exception("head is null.");
		
		if (! head.matches(predicatePattern)) {
			LOG.info("Head does not match predicate pattern. Head: " + head);
			predDefined = null;
			predNecessary = null;
		}
		else {
			predDefined       = head.replaceAll(predicatePattern, "$1");
			predNecessary     = head.replaceAll(predicatePattern, "$2");
			// 	String predOthers = head.replaceAll(predicatePattern, "$3");
		}

	}

	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////

	private void parseFuzzyBody() throws Exception {
		LOG.info("parseFuzzyBody. fuzzyBody: " + fuzzyBody);
		if (fuzzyBody == null) throw new Exception("fuzzy body is null.");
		
		fuzzyBody = removeSpacesBeforeAndAfter(fuzzyBody);
		
		int indexStart = fuzzyBody.indexOf("(");
		int indexMiddle = -1; 
		int indexEnd = -1;
		boolean error = false;
		
		if (indexStart == -1) throw new Exception("fuzzy body contains no functionality.");
		
		String functionality = removeSpacesBeforeAndAfter(fuzzyBody.substring(0, indexStart));
		String arguments = removeSpacesBeforeAndAfter(fuzzyBody.substring(indexStart +1)); 
		
		LOG.info("\n" + "functionality: " + functionality + "\n" + "arguments: " + arguments);
		
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
			indexMiddle = arguments.indexOf(",");
			if (indexMiddle > 1) {
				predNecessary = arguments.substring(0, indexMiddle);
			}
			
			indexStart = arguments.indexOf("[");
			indexEnd = arguments.indexOf("]");
			
			String functionPointsString = "";
			if ((indexStart > -1) && (indexEnd > -1) && (indexStart +1 < arguments.length())) {
				functionPointsString = arguments.substring(indexStart +1, indexEnd);
			}
			arguments = arguments.substring(indexEnd +1);
			
			indexEnd = arguments.indexOf(")");
			if ((indexEnd > -1) && (indexEnd +1 < arguments.length())) 
				arguments.substring(indexEnd +1);
			else arguments = "";
			
			LOG.info("\n" + "functionPointsString: " + functionPointsString + "\n" + "arguments: " + arguments);
			boolean morePoints = true;
			String pointsPairString = "";
			while (morePoints) {
				indexStart = 0;
				indexEnd = functionPointsString.indexOf(")");
				if (indexEnd > -1) {

					pointsPairString = functionPointsString.substring(0, indexEnd);
					if ((indexEnd +1) > functionPointsString.length()) {
						functionPointsString = functionPointsString.substring(indexEnd +1);
					}
					else functionPointsString = "";
					LOG.info("pointsPairString: " + pointsPairString);
					
					indexStart = pointsPairString.indexOf("(");
					if (indexStart > -1) {
						indexMiddle = pointsPairString.indexOf(",");
						if (indexMiddle > -1) {
							String pointX = pointsPairString.substring(indexStart +1, indexMiddle);
							String pointY = pointsPairString.substring(indexMiddle +1);
							
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
				}
				else {
					// Finished reading the points sequence.
					morePoints = false;
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
				if (indexEnd > 0) { // If it is zero we have finished !!!.
					ruleBody += arguments.substring(0, indexEnd);
					indexStart = (arguments.substring(0, indexEnd -1)).indexOf("(");
					if (indexStart == -1) {
						moreBody = false;
					}
					arguments = arguments.substring(indexEnd +1);
				}
				else {
					moreBody = false;
					if (indexEnd == 0) {
						arguments = arguments.substring(indexEnd +1);
					}
				}
			}
		}

		// For defaults.
		if ("defaults_to".equals(functionality)) {
			indexEnd = arguments.indexOf(")");
			if (indexEnd > -1) {
				String defaults_to_str = arguments.substring(0, indexEnd);
				try {
					defaults_to = Float.parseFloat(defaults_to_str);
				}
				catch (Exception e) {
					defaults_to = -1;
				}
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
		LOG.info("extraOption: " + extraOption + " arguments: " + arguments);
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
		if ((predOwner != null) && (! DEFAULT_DEFINITION.equals(predOwner)) && (! "".equals(predOwner))) {
			only_for_user = predOwner;
		}
	}
	
	public String getPredOwner() {
		if ((only_for_user == null) || (only_for_user != null) && ("".equals(only_for_user))) return DEFAULT_DEFINITION;
		else return only_for_user;
	}

	public String getHead() {
		return head;
	}
	
	public String getBody() {
		return body;
	}
	
	public float getValue() {
		return value;
	}
	
	public String getRuleBody() {
		return ruleBody;
	}
	
	public String getRuleAggregator() {
		return ruleAggregator;
	}

	public float getDefaultsTo() {
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
