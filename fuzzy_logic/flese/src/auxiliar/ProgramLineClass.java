package auxiliar;

public class ProgramLineClass {
	private String line = null;
	private FunctionAnalizedClass functionAnalized = null;
	
	ProgramLineClass(String line) {
		
		this.line = line;
		
		// out.println("// " + line + "\n");
		if (line.startsWith("rfuzzy_fuzzification")) {
			try {
				functionAnalized = new FunctionAnalizedClass(line);
			}
			catch (Exception e) {
				functionAnalized = null;
			}
		}
	}
		
	public String getLine () {
		return line;
	}
	
	public FunctionAnalizedClass getFunctionAnalized () {
		return functionAnalized;
	}
	
}
