package auxiliar;

import java.io.*;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class ProgramAnalizedClass {
	final Log LOG = LogFactory.getLog(ProgramAnalizedClass.class);
	
	private String filePath = null;
	private ArrayList <ProgramLineClass> programLines = null;
	private ArrayList <ArrayList <FunctionAnalizedClass>> programFunctionsOrdered = null;
	private Iterator <Iterator <FunctionAnalizedClass>> programFunctionsOrderedIterator = null;
	private String [] programFunctionsOrderedInJavaScript = null;

	public ProgramAnalizedClass (String filePath, String fuzzification) throws Exception {
		
		this.filePath = filePath;
		programFunctionsOrdered = null;
		programFunctionsOrderedIterator = null;
		programFunctionsOrderedInJavaScript = null;
		
		programFunctionsOrdered = new ArrayList <ArrayList <FunctionAnalizedClass>>();
		programLines = new ArrayList <ProgramLineClass>();
		
		if ((filePath != null) && ( ! ("".equals(filePath)))) {
			BufferedReader reader = new BufferedReader(new FileReader(filePath));
			
			String line;
			ProgramLineClass programLine = null;
			
			while ((line = reader.readLine()) != null) {
				programLine = new ProgramLineClass(line);
				programLines.add(programLine);
				
				if (programLine.getFunctionAnalized() != null) {
					addToFunctionsOrderedList(programLine.getFunctionAnalized(), fuzzification);
				}
			}
			reader.close();
		}
	}
	
	private void addToFunctionsOrderedList (FunctionAnalizedClass function, String fuzzification) throws Exception {
		
		if (function == null) throw new Exception("function cannot be null.");

		if ((fuzzification == null) || 
				((fuzzification != null) && (function.getPredDefined().equals(fuzzification)))) {

			int i=0;
			boolean placed = false;
			ArrayList <FunctionAnalizedClass> current = null;

			while ((i<programFunctionsOrdered.size()) && (! placed)) {
				current = programFunctionsOrdered.get(i);
				if ((current != null) && (current.size() > 0)) {
					if (current.get(0).getPredDefined().equals(function.getPredDefined())) {
						current.add(function);
						placed = true;
					}
				}
				i++;
			}
			if (! placed) {
				current = new ArrayList <FunctionAnalizedClass>();
				current.add(function);
				programFunctionsOrdered.add(current);
				placed = true;
			}
			// if (placed) programFunctionsOrderedIterator = null; // Reject last saved result.
		}
	}
	
	
	public Iterator <Iterator <FunctionAnalizedClass>> getResult() {
		if (programFunctionsOrderedIterator == null) {
			ArrayList <Iterator <FunctionAnalizedClass>> tmp = new ArrayList <Iterator <FunctionAnalizedClass>>();
			Iterator <FunctionAnalizedClass> current = null;
			int i = 0;
			while (i<programFunctionsOrdered.size()) {
				current = programFunctionsOrdered.get(i).iterator();
				tmp.add(current);
				i++;
			}
			programFunctionsOrderedIterator = tmp.iterator();
		}
		return programFunctionsOrderedIterator;
	}
	
	public String [] getResultInJavaScript() throws Exception {
		String tmp = null;
		FunctionAnalizedClass function = null;
		ArrayList<FunctionAnalizedClass> current = null;
		
		if (programFunctionsOrderedInJavaScript == null) {
			programFunctionsOrderedInJavaScript = new String[programFunctionsOrdered.size()];
			int i = 0;
			while (i<programFunctionsOrdered.size()) {
				current = programFunctionsOrdered.get(i);
				int j = 0;
				while (j<current.size()) {
					function = current.get(j);
					
					if (j==0) {
						tmp = "new fuzzificationDef('" + function.getPredDefined() + "', '" + function.getPredNecessary() + "', ";
						tmp += "new Array(";
					}
					else tmp += ", ";
					
					tmp += "new fuzzificationFunctionDef('" + function.getPredOwner() + "', " + function.getFunctionInJavaScript() + ")";
					j++;
				}
				tmp += "))"; // End the javascript arrays.
				programFunctionsOrderedInJavaScript[i] = tmp;
				i++;
			}
		}
		
		return programFunctionsOrderedInJavaScript;
	}
	
	// out.println("// line: "             + line + "\n");
	// out.println("personalizePredInfo["+i+"]= new Array('" + 
	//		function.getPredDefined() + "', '" + function.getPredNecessary() + "', '" + function.getPredOwner() + "', " + 
	//		function.getFunctionInJavaScript() + "); \n");
	// out.println(line);
	// out.print("<br />\n");

	
	public void updateProgramFile(String fuzzification, LocalUserNameClass localUserName, String [] [] params) throws IOException {
		ProgramLineClass programLine = null;
		
		File file = new File(filePath);
		if (file.exists()) {
			String filePathAux = filePath;
			
			DateFormat dateFormat = new SimpleDateFormat("backup_yyyyMMdd_HHmmss");
			Date date = new Date();
			// System.out.println(dateFormat.format(date));
			
			// Remove the extension.
			if (filePathAux.endsWith(".pl"))  filePathAux = filePathAux.substring(0, filePathAux.length() - ".pl".length());
			
			// Add the new suffix.
			filePathAux = filePathAux + dateFormat.format(date);
			
			// Rename the original file.
			File fileAux = new File(filePathAux);
			file.renameTo(fileAux);
		}
		
		// Create a new file.
		file.createNewFile();

		ArrayList <ProgramLineClass> programLinesAffected = new ArrayList <ProgramLineClass>();
		boolean foundFuzzifications = false;
		boolean copiedBackFuzzifications = false;
		
		FileWriter fw = new FileWriter(file.getAbsoluteFile());
		BufferedWriter bw = new BufferedWriter(fw);
		
		for (int i=0; i<programLines.size(); i++) {
			programLine = programLines.get(i);
			
			if (! copiedBackFuzzifications) {
				if ((programLine.getFunctionAnalized() != null) && 
						(programLine.getFunctionAnalized().getPredDefined().equals(fuzzification))) {
					foundFuzzifications = true;
					if (! programLine.getFunctionAnalized().getPredOwner().equals(localUserName)) {
						programLinesAffected.add(programLine);
					}
				}
				else {
					if (foundFuzzifications) {
						copiedBackFuzzifications = true;
						
						String predNecessary = null;
						for (int j=0; j<programLinesAffected.size(); j++) {
							predNecessary = programLinesAffected.get(j).getFunctionAnalized().getPredNecessary();
							bw.write(programLinesAffected.get(j).getLine());
						}
						String newLine = buildFuzzificationLine(fuzzification, predNecessary, localUserName, params);
						LOG.info("Adding to the program the generated line: \n " + newLine);
						bw.write(newLine);
					}
				}
			}
			
			bw.write(programLine.getLine());
		}
		
		bw.close();
	}
	
	private String buildFuzzificationLine(String fuzzification, String predNecessary, LocalUserNameClass localUserName, String [] [] params) {
		// example: 
		// rfuzzy_fuzzification(traditional(restaurant), years_since_opening(restaurant), bartolo) :- function([ (0, 1), (5, 0.2), (10, 0.8), (15, 1), (100, 1) ]).

		String result = "([ ";
		for (int i=0; i<params.length; i++) {
			result += "(" + params[i][0] + ", " + params[i][1] + ")";
			if (i+1 < params.length) result += ", ";
		}
		result += " ]).";
		result = "rfuzzy_fuzzification(" + fuzzification + ", " + predNecessary + ", " + localUserName.getLocalUserName() + ") :- function" + result;
		
		return "result";
	}
}



/* EOF */
