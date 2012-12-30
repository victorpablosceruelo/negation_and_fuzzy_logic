package auxiliar;

import java.util.ArrayList;

public class ProgramPartAnalysisClass {
	private ArrayList <String> programSubPartLines = null;
	private ArrayList <String> programSubPartComments = null;
	
	private String programSubPartLine = null;
	private String head = null;
	private String body = null;
	private String fuzzyBody = null;
	private boolean partIsIncomplete = false;
	
	
	ProgramPartAnalysisClass() {
		this.programSubPartLines = new ArrayList <String>();
		this.programSubPartComments = new ArrayList <String>();
	}
	
	public String parse(String programLine) throws Exception {
		
		if (programLine == null) throw new Exception("program line is null");

		
		int index = programLine.indexOf("%");
		if (index > -1) {
			programSubPartComments.add(programLine.substring(index));
			programLine = programLine.substring(0, index);
		}
		else programSubPartComments.add(null);
		
		index = programLine.indexOf(".");
		if (index > -1) {
			programSubPartLine = programLine.substring(0, index);
			programLine = programLine.substring(index);
			partIsIncomplete = true;
		}
		else {
			programSubPartLine = programLine.substring(0);
			programLine = "";
			partIsIncomplete = false;
		}
		programSubPartLines.add(programSubPartLine);
		
			
		index = programSubPartLine.indexOf(":-");
		if (index > -1) {
			head = programLine.substring(0, index);
			body = programLine.substring(0, index);
			programSubPartLine = "";
		}

		index = programSubPartLine.indexOf(":~");
		if (index > -1) {
			head = programLine.substring(0, index);
			fuzzyBody = programLine.substring(0, index);

			parseHead();
			parseFuzzyBody();
			programSubPartLine = "";
		}

		if (! ("".equals(programSubPartLine))) {
			head = programSubPartLine;
		}

		programLine = removeSpaces(programLine);
		if ("".equals(programLine)) programLine = null;
		return programLine;
	}
	
	private String removeSpaces(String input) {
		if (input == null) return input;
		while ((! "".equals(input)) && (input.charAt(0) == ' ')) {
			input = input.substring(1);
		}
		return input;
	}
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
	
	private void parseHead() {
		
	}
	private void parseFuzzyBody() {
		
	}
}
