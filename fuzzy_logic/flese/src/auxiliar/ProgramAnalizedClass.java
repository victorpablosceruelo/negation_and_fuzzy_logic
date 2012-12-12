package auxiliar;

import java.io.*;
import java.util.ArrayList;
import java.util.Iterator;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class ProgramAnalizedClass {
	final Log LOG = LogFactory.getLog(ProgramAnalizedClass.class);
	
	final private static String DEFAULT_DEFINITION = "default definition";
	
	private String localUserName = null;
	private String fileName = null; 
	private String fileOwner = null;
	private String filePath = null;
	private ArrayList <ProgramLineClass> programLines = null;
	private ArrayList <ArrayList <FunctionAnalizedClass>> programFunctionsOrdered = null;
	private Iterator <Iterator <FunctionAnalizedClass>> programFunctionsOrderedIterator = null;
	private String [] programFunctionsOrderedInJavaScript = null;
	

	/**
	 * Analizes the program file pointed by filePath.
	 * @param localUserName
	 * @param fileName
	 * @param fileOwner
	 * @param filePath
	 * @throws Exception when any of the previous is null or empty string.
	 */
	public ProgramAnalizedClass (String localUserName, String fileName, String fileOwner, String filePath) throws Exception {
		
		if (localUserName == null) throw new Exception("localUserName cannot be null.");
		if (fileName == null) throw new Exception("fileName cannot be null.");
		if (fileOwner == null) throw new Exception("fileOwner cannot be null.");
		if (filePath == null) throw new Exception("filePath cannot be null.");
		
		if ("".equals(localUserName)) throw new Exception("localUserName cannot be an empty string.");
		if ("".equals(fileName)) throw new Exception("fileName cannot be an empty string.");
		if ("".equals(fileOwner)) throw new Exception("fileOwner cannot be an empty string.");
		if ("".equals(filePath)) throw new Exception("filePath cannot be an empty string.");
		
		this.localUserName = localUserName;
		this.fileName = fileName;
		this.fileOwner = fileOwner;
		this.filePath = filePath;
		
		LOG.info("localUserName: " + localUserName + " fileName: " + this.fileName + " fileOwner: " + this.fileOwner + " filePath: " + this.filePath);
		
		programLines = new ArrayList <ProgramLineClass>();
		programFunctionsOrdered = new ArrayList <ArrayList <FunctionAnalizedClass>>();
		programFunctionsOrderedIterator = null;
		programFunctionsOrderedInJavaScript = null;
		
		
		if ((filePath != null) && ( ! ("".equals(filePath)))) {
			BufferedReader reader = new BufferedReader(new FileReader(filePath));
			
			String line;
			ProgramLineClass programLine = null;
			
			while ((line = reader.readLine()) != null) {
				programLine = new ProgramLineClass(line);
				programLines.add(programLine);
				
				if (programLine.getFunctionAnalized() != null) {
					if ((localUserName.equals(fileOwner)) ||
						((programLine.getFunctionAnalized().getPredOwner() != null) && 
						 ((programLine.getFunctionAnalized().getPredOwner().equals(localUserName)) ||
						  (programLine.getFunctionAnalized().getPredOwner().equals(DEFAULT_DEFINITION))))) {
						addToFunctionsOrderedList(programLine.getFunctionAnalized());
					}
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
	
	public String [] getProgramFuzzificationsInJS() throws Exception {
		
		if (programFunctionsOrderedIterator == null) {
			programFunctionsOrderedInJavaScript = null;
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
						tmp = "addFuzzificationFunctionDefinition('" + function.getPredDefined() + "', '" + function.getPredNecessary() + "', ";
						tmp += "new Array(";
					}
					else tmp += ", ";
					
					tmp += "new ownerPersonalization('" + function.getPredOwner() + "', " + function.getFunctionInJavaScript() + ")";
					j++;
				}
				tmp += "))"; // End the javascript arrays.
				programFunctionsOrderedInJavaScript[i] = tmp;
				i++;
			}
		}
		
		return programFunctionsOrderedInJavaScript;
	}

	
	public void updateProgramFile(String fuzzification, String fuzzificationOwner, String [] [] params) throws Exception {
		
		LOG.info("saving fuzzification: " + fuzzification + " personalized for username: " + fuzzificationOwner + " by username: " + localUserName + "\n\n");
		
		// Security issues.
		if (fuzzificationOwner == null) fuzzificationOwner = localUserName;
		if (fuzzificationOwner == null) throw new Exception ("fuzzificationOwner cannot be null.");
		if ("".equals(fuzzificationOwner)) throw new Exception ("fuzzificationOwner cannot be empty string.");

		// If I'm not the owner I can change only my fuzzification.
		// If I'm the owner I can change mine and the default one, but no other one.
		if ((! localUserName.equals(fileOwner)) || (! fuzzificationOwner.equals(DEFAULT_DEFINITION))) {
			fuzzificationOwner = localUserName;
		}
		
		ProgramLineClass programLine = null;
		
		File file = new File(filePath);
		if (file.exists()) {
			String filePathAux = filePath;
			
			// Remove the extension.
			if (filePathAux.endsWith(".pl"))  filePathAux = filePathAux.substring(0, filePathAux.length() - ".pl".length());
			
			// Add the new suffix.
			filePathAux = filePathAux + "_backup_" + ServletsAuxMethodsClass.getCurrentDate() + ".txt";
			
			// Rename the original file.
			LOG.info("renaming " + filePath + " into " + filePathAux);
			File fileAux = new File(filePathAux);
			file.renameTo(fileAux);
		}
		
		// Create a new file.
		LOG.info("creating the file " + filePath);
		file.createNewFile();

		ArrayList <ProgramLineClass> programLinesAffected = new ArrayList <ProgramLineClass>();
		boolean foundFuzzifications = false;
		boolean copiedBackFuzzifications = false;
		
		FileWriter fw = new FileWriter(file.getAbsoluteFile());
		BufferedWriter bw = new BufferedWriter(fw);
		
		int j=0;
		String predNecessary = null;
		boolean found = false;
		
		for (int i=0; i<programLines.size(); i++) {
			programLine = programLines.get(i);
			// LOG.info("analizing the program line: " + programLine.getLine());
			
			if (! copiedBackFuzzifications) {
				if ((programLine.getFunctionAnalized() != null) && 
						(programLine.getFunctionAnalized().getPredDefined().equals(fuzzification))) {
					foundFuzzifications = true;
					programLinesAffected.add(programLine);
				}
				else {
					if (foundFuzzifications) {
						copiedBackFuzzifications = true;

						// First the default definition.
						found = false;
						while ((j < programLinesAffected.size()) && (! found)) {
							if (programLinesAffected.get(j).getFunctionAnalized().getPredOwner().equals(DEFAULT_DEFINITION)) found = true;
							else j++;
						}
						// Initialize predNecessary value.
						predNecessary = programLinesAffected.get(j).getFunctionAnalized().getPredNecessary();
						
						if (fuzzificationOwner.equals(DEFAULT_DEFINITION)) {	
							bw.write(buildFuzzificationLine(fuzzification, predNecessary, fuzzificationOwner, params));
						}
						else {
							bw.write(programLinesAffected.get(j).getLine() + "\n");
						}
						
						// Now the other ones except the one we are playing with.
						j=0;
						while (j < programLinesAffected.size()) {
							if ((! programLinesAffected.get(j).getFunctionAnalized().getPredOwner().equals(DEFAULT_DEFINITION)) &&
								(! programLinesAffected.get(j).getFunctionAnalized().getPredOwner().equals(fuzzificationOwner))) {
								bw.write(programLinesAffected.get(j).getLine() + "\n");
							}
							j++;
						}
						
						// Now the one we are playing with, if it is not the default one.
						if (! fuzzificationOwner.equals(DEFAULT_DEFINITION)) {
							bw.write(buildFuzzificationLine(fuzzification, predNecessary, fuzzificationOwner, params));
						}						
					}
				}
			}
			
			if ((! foundFuzzifications) || (foundFuzzifications && copiedBackFuzzifications)) {
				bw.write(programLine.getLine() + "\n");
			}
		}
		
		bw.close();
		
		// At this point we must clear the CiaoPrologConnection queries cache, but introducing the parameter session
		// or the parameter CiaoPrologConnectionClass complicates this innecessarily. We must do it in the servlet
		// that calls us.
	}
	
	private String buildFuzzificationLine(String fuzzification, String predNecessary, String fuzzificationOwner, String [] [] params) throws Exception {
		// example: 
		// rfuzzy_fuzzification(traditional(restaurant), years_since_opening(restaurant), bartolo) :- function([ (0, 1), (5, 0.2), (10, 0.8), (15, 1), (100, 1) ]).

		if (fuzzification == null) throw new Exception ("fuzzification cannot be null.");
		if (predNecessary == null) throw new Exception ("predNecessary cannot be null.");
		if (fuzzificationOwner == null) throw new Exception ("fuzzificationOwner cannot be null.");
		if (params == null) throw new Exception ("params cannot be null.");
		
		if ("".equals(fuzzification)) throw new Exception ("fuzzification cannot be empty string.");
		if ("".equals(predNecessary)) throw new Exception ("predNecessary cannot be empty string.");
		if ("".equals(fuzzificationOwner)) throw new Exception ("fuzzificationOwner cannot be empty string.");
		
		
		String fuzzificationFunction = "function([ ";
		for (int i=0; i<params.length; i++) {
			fuzzificationFunction += "(" + params[i][0] + ", " + params[i][1] + ")";
			if (i+1 < params.length) fuzzificationFunction += ", ";
		}
		fuzzificationFunction += " ]).";
		
		String fuzzificationHead = "rfuzzy_fuzzification(" + fuzzification + ", " + predNecessary;
		if (! fuzzificationOwner.equals(DEFAULT_DEFINITION)) fuzzificationHead += ", " + fuzzificationOwner;
		fuzzificationHead += ")"; 
		
		String result = fuzzificationHead + " :- " + fuzzificationFunction + " \n";
		LOG.info("Line generated: \n " + result);
		return result;
	}
}

/*
 * 
 */

/* EOF */

