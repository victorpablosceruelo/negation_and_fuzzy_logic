package auxiliar;

import java.io.*;
import java.util.ArrayList;

public class FunctionsClass {
	
	String [] result = null;
	
	ArrayList <ArrayList <FunctionClass>> functionsOrdered = null;

	public FunctionsClass (String filePath) throws Exception {
		
		functionsOrdered = new ArrayList <ArrayList <FunctionClass>>();
		
		if ((filePath != null) && ( ! ("".equals(filePath)))) {
			BufferedReader reader = new BufferedReader(new FileReader(filePath));
			
			String line;
			while ((line = reader.readLine()) != null) {
				// out.println("// " + line + "\n");
				if (line.startsWith("rfuzzy_fuzzification")) {
					FunctionClass function = new FunctionClass(line);
					addInItsList(function);
				}
			}
			reader.close();
		}
	}
	
	private void addInItsList (FunctionClass function) {
		
		int i=0;
		boolean placed = false;
		
		while ((i<functionsOrdered.size()) && (! placed)) {
			
			
			for (i=0; i<functionsOredered.size(); i++) {
				
			}
		}
		if (! placed) {
			ArrayList <FunctionClass> functions = new ArrayList <FunctionClass>();
			functions.add(function);
			functionsOrdered.add(functions);
		}
	}
	
	
	public String [] getResult() {
		return result;
		// out.println("// line: "             + line + "\n");
		// out.println("personalizePredInfo["+i+"]= new Array('" + 
		//		function.getPredDefined() + "', '" + function.getPredNecessary() + "', '" + function.getPredOwner() + "', " + 
		//		function.getFunctionInJavaScript() + "); \n");
		// out.println(line);
		// out.print("<br />\n");

	}
}
