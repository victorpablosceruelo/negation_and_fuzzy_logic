package auxiliar;

public class ProgramLineClass {
	private int index = 0;
	private String line = null;
	private FunctionAnalizedClass functionAnalized = null;
	
	ProgramLineClass(int index, String line) {
		
		this.line = line;
		this.index = index;
		
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
	
	public int getIndex () {
		return index;
	}
	
	public String getLine () {
		return line;
	}
	
	public FunctionAnalizedClass getFunctionAnalized () {
		return functionAnalized;
	}
	
}
