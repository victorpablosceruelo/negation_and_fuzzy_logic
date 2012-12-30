package auxiliar;

import java.util.ArrayList;

public class ProgramPartAnalysisClass {
	private ArrayList <String> programSubPartLines = null;
	private int programSubPartType = 0;
	
	ProgramPartAnalysisClass() {
		this.programSubPartLines = new ArrayList <String>();
		this.programSubPartType = 0;
	}
	
	public void parse(String line) {
		
		if (line == null) throw new Exception("line is null");
		
		programSubPartLines.add(line);
		
		
			functionAnalized = new FunctionAnalizedClass(line);
	}
		
	public String getLine () {
		return line;
	}
	
	public FunctionAnalizedClass getFunctionAnalized () {
		return functionAnalized;
	}
	
}
