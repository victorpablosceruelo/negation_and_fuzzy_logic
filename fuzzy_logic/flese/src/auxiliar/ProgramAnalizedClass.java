package auxiliar;

import java.io.*;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;

public class ProgramAnalizedClass {
	
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
			int index = 0;
			
			while ((line = reader.readLine()) != null) {
				programLine = new ProgramLineClass(index, line);
				programLines.add(programLine);
				
				if ((fuzzification == null) || 
					((fuzzification != null) && (programLine.getFunctionAnalized().getPredDefined().equals(fuzzification)))) {
					addToFunctionsOrderedList(programLine.getFunctionAnalized());
				}
			}
			reader.close();
		}
	}
	
	private void addToFunctionsOrderedList (FunctionAnalizedClass function) throws Exception {
		
		if (function == null) throw new Exception("function cannot be null.");
		
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
		
		for (int index=0; index<programLines.size(); index++) {
			programLine = programLines.get(index);
			
			if ((programLine.getFunctionAnalized() != null) && 
				(programLine.getFunctionAnalized().getPredDefined().equals(fuzzification))) {
				String newLine = buildFuzzificationLine(fuzzification, localUserName, )
				bw.write(newLine);
				previouslyWritten = true;
			}
			
			bw.write(programLine.getLine());
		}
		
		bw.close();
	}
	
	
}



/* EOF */
