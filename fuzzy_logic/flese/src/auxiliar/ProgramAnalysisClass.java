package auxiliar;

import java.io.*;
import java.util.ArrayList;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class ProgramAnalysisClass {
	final Log LOG = LogFactory.getLog(ProgramAnalysisClass.class);
	
	private String localUserName = null;
	private String fileName = null; 
	private String fileOwner = null;
	private String filePath = null;
	private ArrayList <ProgramPartAnalysisClass> programParts = null;
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
		
		LOG.info("\n localUserName: " + localUserName + "\n fileName: " + this.fileName + 
				 "\n fileOwner: " + this.fileOwner + "\n filePath: " + this.filePath);
		
		programParts = new ArrayList <ProgramPartAnalysisClass>();
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
		
		String msg = "----------------------------------------------------------------------";
		LOG.info("\n" + msg + "\n" + msg + "\n" + msg + "\n");
		
	}
	
	public String [] getProgramFuzzificationsInJS() throws Exception {
		
		LOG.info("getProgramFuzzificationsInJS");
		if (programParts == null) throw new Exception("programParts is null.");
		
		// Use the information cached.
		if (programFunctionsOrderedInJavaScript != null) {
			return programFunctionsOrderedInJavaScript;
		}
		
		ArrayList <ArrayList <ProgramPartAnalysisClass>> programFunctionsOrdered = new ArrayList <ArrayList <ProgramPartAnalysisClass>>();
		ProgramPartAnalysisClass programPart = null;
		int i=0, j=0;
		
		for (i=0; i<programParts.size(); i++) {
			programPart = programParts.get(i);
			if (programPart.isFunction()) {
				if (	(localUserName.equals(fileOwner)) || 
						(programPart.getPredOwner().equals(localUserName)) || 
						(programPart.getPredOwner().equals(ProgramPartAnalysisClass.DEFAULT_DEFINITION))   ) {

					boolean placed = false;

					j=0;				
					while ((j<programFunctionsOrdered.size()) && (! placed)) {
						if ((programFunctionsOrdered.get(j) != null) && (programFunctionsOrdered.get(j).size() > 0)) {
							if (	(programFunctionsOrdered.get(j).get(0).getPredDefined().equals(programPart.getPredDefined())) &&
									(programFunctionsOrdered.get(j).get(0).getPredNecessary().equals(programPart.getPredNecessary()))) {
								programFunctionsOrdered.get(j).add(programPart);
								placed = true;
							}
						}
						j++;
					}
					if (! placed) {
						ArrayList <ProgramPartAnalysisClass> current = new ArrayList <ProgramPartAnalysisClass>();
						current.add(programPart);
						programFunctionsOrdered.add(current);
						placed = true;
					}
				}
			}
/*			else {
				LOG.info("Not a function. programPart.getHead(): " + programPart.getHead());
				String [] lines = programPart.getLines();
				String line = "";
				for (int k = 0; k< lines.length; k++) {
					line += lines[k] + "\n";
				}
				LOG.info(line);
			}
*/
		}	
		
		String tmp = null;
		ProgramPartAnalysisClass function = null;
		programFunctionsOrderedInJavaScript = new String[programFunctionsOrdered.size()];
		i = 0;
		LOG.info("programFunctionsOrdered.size(): " + programFunctionsOrdered.size());
		while (i<programFunctionsOrdered.size()) {
			j = 0;
			LOG.info("programFunctionsOrdered.get(i).size(): " + programFunctionsOrdered.get(i).size());
			while (j<programFunctionsOrdered.get(i).size()) {
				function = programFunctionsOrdered.get(i).get(j);

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
		
		return programFunctionsOrderedInJavaScript;
	}

	
	public void updateProgramFile(String predDefined, String predNecessary, String predOwner, String [] [] params) throws Exception {
		
		LOG.info("saving fuzzification: " + predDefined + " depending on " + predNecessary + " personalized for username: " + predOwner + " by username: " + localUserName + "\n\n");
		
		// Security issues (null pointers).
		if (predDefined == null) throw new Exception ("predDefined cannot be null.");
		if (predNecessary == null) throw new Exception ("predNecessary cannot be null.");
		if (predOwner == null) throw new Exception ("predOwner cannot be null.");
		
		// Security issues ("" strings).
		if ("".equals(predDefined)) throw new Exception ("predDefined cannot be empty string.");
		if ("".equals(predNecessary)) throw new Exception ("predNecessary cannot be empty string.");
		if ("".equals(predOwner)) throw new Exception ("predOwner cannot be empty string.");

		// If I'm not the owner I can change only my fuzzification.
		// If I'm the owner I can change mine and the default one, but no other one.
		if (	(! localUserName.equals(fileOwner)) || 
				(! predOwner.equals(ProgramPartAnalysisClass.DEFAULT_DEFINITION))) {
			predOwner = localUserName;
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
		
		int j=0, k=0;
		boolean updated = false;
		String [] lines = null;
		
		for (int i=0; i<programParts.size(); i++) {
			programPart = programParts.get(i);
			// LOG.info("analyzing the program line: " + programPart.getLine());
			
			if (! copiedBackFuzzifications) {
				if ((programPart.isFunction()) && (programPart.getPredDefined().equals(predDefined)) && (programPart.getPredNecessary().equals(predNecessary))) {
					foundFuzzifications = true;
					programPartsAffected.add(programPart);
				}
				else {
					if (foundFuzzifications) {
						copiedBackFuzzifications = true;

						updated = false;
						for (j=0; j < programPartsAffected.size(); j++) {
							if (programPartsAffected.get(j).getPredOwner().equals(predOwner)) {
								programPartsAffected.get(j).updateFunction(params);
								updated = true;
							}

							lines = programPartsAffected.get(j).getLines();
							for (k = 0; k< lines.length; k++) {
								bw.write(lines[k] + "\n");
							}
						}

						if (! updated) {
							ProgramPartAnalysisClass newProgramPart = new ProgramPartAnalysisClass();
							newProgramPart.setPredDefined(predDefined);
							newProgramPart.setPredNecessary(predNecessary);
							newProgramPart.setPredOwner(predOwner);
							newProgramPart.updateFunction(params);
							
							lines = programPartsAffected.get(j).getLines();
							for (k = 0; k< lines.length; k++) {
								bw.write(lines[k] + "\n");
							}
						}						
					}
				}
			}
			
			if ((! foundFuzzifications) || (foundFuzzifications && copiedBackFuzzifications)) {
				lines = programPart.getLines();
				for (k = 0; k< lines.length; k++) {
					bw.write(lines[k] + "\n");
				}
			}
		}
		
		bw.close();
		
		programFunctionsOrderedInJavaScript = null;
		// At this point we must clear the CiaoPrologConnection queries cache, but introducing the parameter session
		// or the parameter CiaoPrologConnectionClass complicates this innecessarily. We must do it in the servlet
		// that calls us.
	}

}

/*
 * 
 */

/* EOF */

