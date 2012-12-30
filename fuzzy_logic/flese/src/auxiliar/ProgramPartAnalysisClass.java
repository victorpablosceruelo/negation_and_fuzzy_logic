package auxiliar;

public class ProgramPartAnalysisClass {
	private String line = null;
	private FunctionAnalizedClass functionAnalized = null;
	
	ProgramPartAnalysisClass(String line) {
		
		this.line = line;
		
		try {
			functionAnalized = new FunctionAnalizedClass(line);
		}
		catch (Exception e) {
			functionAnalized = null;
		}
	}
		
	public String getLine () {
		return line;
	}
	
	public FunctionAnalizedClass getFunctionAnalized () {
		return functionAnalized;
	}
	
}
