package auxiliar;

import java.io.*;
import java.util.ArrayList;
import java.util.Iterator;

public class FunctionsClass {
	
	private ArrayList <ArrayList <FunctionClass>> functionsOrdered = null;
	private Iterator <Iterator <FunctionClass>> functionsOrderedIterator = null;
	private String [][] functionsOrderedInJavaScript = null;

	public FunctionsClass (String filePath) throws Exception {
		
		functionsOrdered = null;
		functionsOrderedIterator = null;
		functionsOrderedInJavaScript = null;
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
	
	private void addInItsList (FunctionClass function) throws Exception {
		
		if (function == null) throw new Exception("function cannot be null.");
		
		int i=0;
		boolean placed = false;
		ArrayList <FunctionClass> current = null;
		
		while ((i<functionsOrdered.size()) && (! placed)) {
			current = functionsOrdered.get(i);
			if ((current != null) && (current.size() > 0)) {
				if (current.get(0).getPredDefined().equals(function.getPredDefined())) {
					current.add(function);
					placed = true;
				}
			}
			i++;
		}
		if (! placed) {
			current = new ArrayList <FunctionClass>();
			current.add(function);
			functionsOrdered.add(current);
			placed = true;
		}
		// if (placed) functionsOrderedIterator = null; // Reject last saved result.
	}
	
	
	public Iterator <Iterator <FunctionClass>> getResult() {
		if (functionsOrderedIterator == null) {
			ArrayList <Iterator <FunctionClass>> tmp = new ArrayList <Iterator <FunctionClass>>();
			Iterator <FunctionClass> current = null;
			int i = 0;
			while (i<functionsOrdered.size()) {
				current = functionsOrdered.get(i).iterator();
				tmp.add(current);
			}
			functionsOrderedIterator = tmp.iterator();
		}
		return functionsOrderedIterator;
		// out.println("// line: "             + line + "\n");
		// out.println("personalizePredInfo["+i+"]= new Array('" + 
		//		function.getPredDefined() + "', '" + function.getPredNecessary() + "', '" + function.getPredOwner() + "', " + 
		//		function.getFunctionInJavaScript() + "); \n");
		// out.println(line);
		// out.print("<br />\n");

	}
	
	public String [] getResultInJavaScript() {
		ArrayList <String> result = new ArrayList <String>();
		String tmp = null;
		FunctionClass function = null;
		
		if (functionsOrderedInJavaScript == null) {
			Iterator <Iterator <FunctionClass>> iterator = getResult();
			while (iterator.hasNext()) {
				Iterator <FunctionClass> current = iterator.next();
				while (current.hasNext()) {
					function = current.next();
					result.add(e)
				}
			}
		}
		return functionsOrderedInJavaScript;
	}
	
}
