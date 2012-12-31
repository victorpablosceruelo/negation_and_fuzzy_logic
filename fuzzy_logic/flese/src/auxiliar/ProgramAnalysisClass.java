package auxiliar;

import java.io.*;
import java.util.ArrayList;
import java.util.Iterator;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class ProgramAnalysisClass {
	final Log LOG = LogFactory.getLog(ProgramAnalysisClass.class);
	
	final private static String DEFAULT_DEFINITION = "default definition";
	
	private String localUserName = null;
	private String fileName = null; 
	private String fileOwner = null;
	private String filePath = null;
	private ArrayList <ProgramPartAnalysisClass> programParts = null;
	private ArrayList <ArrayList <ProgramPartLineClass>> programFunctionsOrdered = null;
	private Iterator <Iterator <ProgramPartLineClass>> programFunctionsOrderedIterator = null;
	private String [] programFunctionsOrderedInJavaScript = null;
	

	/**
	 * Analizes the program file pointed by filePath.
	 * @param localUserName
	 * @param fileName
	 * @param fileOwner
	 * @param filePath
	 * @throws Exception when any of the previous is null or empty string.
	 */
	public ProgramAnalysisClass (String localUserName, String fileName, String fileOwner, String filePath) throws Exception {
		
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
		
		programParts = new ArrayList <ProgramPartAnalysisClass>();
		programFunctionsOrdered = new ArrayList <ArrayList <ProgramPartLineClass>>();
		programFunctionsOrderedIterator = null;
		programFunctionsOrderedInJavaScript = null;
		
		
		if ((filePath != null) && ( ! ("".equals(filePath)))) {
			BufferedReader reader = new BufferedReader(new FileReader(filePath));
			
			String line;
			ProgramPartAnalysisClass programPart = null;
			boolean continues = false;
			
			while ((line = reader.readLine()) != null) {
				while (line != null) {
					if (! continues) {
						programPart = new ProgramPartAnalysisClass();
					}
					line = programPart.parse(line);
				}
				continues = programPart.partIsIncomplete();
				if (! continues) programParts.add(programPart);
			}
			reader.close();
		}
	}
	
	private void addToFunctionsOrderedList (ProgramPartLineClass function) throws Exception {
		
		if (function == null) throw new Exception("function cannot be null.");

		int i=0;
		boolean placed = false;
		ArrayList <ProgramPartLineClass> current = null;

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
			current = new ArrayList <ProgramPartLineClass>();
			current.add(function);
			programFunctionsOrdered.add(current);
			placed = true;
		}
		// if (placed) programFunctionsOrderedIterator = null; // Reject last saved result.
	}
	
	public String [] getProgramFuzzificationsInJS() throws Exception {
		
		if (programFunctionsOrderedIterator == null) {
			programFunctionsOrderedInJavaScript = null;
			ArrayList <Iterator <ProgramPartLineClass>> tmp = new ArrayList <Iterator <ProgramPartLineClass>>();
			Iterator <ProgramPartLineClass> current = null;
			int i = 0;
			while (i<programFunctionsOrdered.size()) {
				current = programFunctionsOrdered.get(i).iterator();
				tmp.add(current);
				i++;
			}
			programFunctionsOrderedIterator = tmp.iterator();
		}
		
		String tmp = null;
		ProgramPartLineClass function = null;
		ArrayList<ProgramPartLineClass> current = null;
		
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
		
		ProgramPartAnalysisClass programPart = null;
		
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

		ArrayList <ProgramPartAnalysisClass> programPartsAffected = new ArrayList <ProgramPartAnalysisClass>();
		boolean foundFuzzifications = false;
		boolean copiedBackFuzzifications = false;
		
		FileWriter fw = new FileWriter(file.getAbsoluteFile());
		BufferedWriter bw = new BufferedWriter(fw);
		
		int j=0;
		String predNecessary = null;
		boolean found = false;
		
		programParts.
		
		for (int i=0; i<programParts.size(); i++) {
			programPart = programParts.get(i);
			// LOG.info("analyzing the program line: " + programPart.getLine());
			
			if (! copiedBackFuzzifications) {
				if ((programPart.isFunction()) && (programPart.getPredDefined().equals(fuzzification))) {
					foundFuzzifications = true;
					programPartsAffected.add(programPart);
				}
				else {
					if (foundFuzzifications) {
						copiedBackFuzzifications = true;

						// First the default definition.
						found = false;
						while ((j < programPartsAffected.size()) && (! found)) {
							if (programPartsAffected.get(j).getPredOwner().equals(DEFAULT_DEFINITION)) found = true;
							else j++;
						}
						// Initialize predNecessary value.
						predNecessary = programPartsAffected.get(j).getPredNecessary();
						
						if (fuzzificationOwner.equals(DEFAULT_DEFINITION)) {	
							bw.write(buildFuzzificationLine(fuzzification, predNecessary, fuzzificationOwner, params));
						}
						else {
							bw.write(programPartsAffected.get(j).getLine() + "\n");
						}
						
						// Now the other ones except the one we are playing with.
						j=0;
						while (j < programPartsAffected.size()) {
							if ((! programPartsAffected.get(j).getFunctionAnalized().getPredOwner().equals(DEFAULT_DEFINITION)) &&
								(! programPartsAffected.get(j).getFunctionAnalized().getPredOwner().equals(fuzzificationOwner))) {
								bw.write(programPartsAffected.get(j).getLine() + "\n");
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
				bw.write(programPart.getLine() + "\n");
			}
		}
		
		bw.close();
		
		// At this point we must clear the CiaoPrologConnection queries cache, but introducing the parameter session
		// or the parameter CiaoPrologConnectionClass complicates this innecessarily. We must do it in the servlet
		// that calls us.
	}

}

/*
 * 
 */

/* EOF */

