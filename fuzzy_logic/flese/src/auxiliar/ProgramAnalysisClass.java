package auxiliar;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.util.ArrayList;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import storeHouse.RequestStoreHouse;
import filesAndPaths.ProgramFileInfo;

public class ProgramAnalysisClass {
	final Log LOG = LogFactory.getLog(ProgramAnalysisClass.class);

	private ProgramFileInfo programFileInfo;
	private ArrayList<ProgramPartAnalysisClass> programParts = null;
	private String[] programFunctionsOrderedInJavaScript = null;

	/**
	 * Analizes the program file pointed by filePath.
	 * 
	 * @param requestStoreHouse
	 * @throws Exception
	 *             when any of the previous is null or empty string.
	 */
	public ProgramAnalysisClass(ProgramFileInfo programFileInfo) throws Exception {

		this.programFileInfo = programFileInfo;

		LOG.info("\n filePath: " + programFileInfo.getProgramFileFullPath());

		programParts = new ArrayList<ProgramPartAnalysisClass>();
		programFunctionsOrderedInJavaScript = null;

		BufferedReader reader = new BufferedReader(new FileReader(programFileInfo.getProgramFileFullPath()));

		String line;
		ProgramPartAnalysisClass programPart = null;
		boolean continues = false;

		while ((line = reader.readLine()) != null) {
			while (line != null) {
				if (!continues) {
					programPart = new ProgramPartAnalysisClass();
				}
				line = programPart.parse(line);
			}
			continues = programPart.partIsIncomplete();
			if (!continues)
				programParts.add(programPart);
		}
		reader.close();

	}

	public String[] getProgramFuzzificationsInJS(LocalUserInfo localUserInfo) throws Exception {

		LOG.info("getProgramFuzzificationsInJS");
		if (programParts == null)
			throw new Exception("programParts is null.");

		// Use the information cached.
		if (programFunctionsOrderedInJavaScript != null) {
			return programFunctionsOrderedInJavaScript;
		}

		ArrayList<ArrayList<ProgramPartAnalysisClass>> programFunctionsOrdered = new ArrayList<ArrayList<ProgramPartAnalysisClass>>();
		ProgramPartAnalysisClass programPart = null;
		int i = 0, j = 0;

		for (i = 0; i < programParts.size(); i++) {
			programPart = programParts.get(i);
			if (programPart.isFunction()) {
				if ((localUserInfo.getLocalUserName().equals(programFileInfo.getFileOwner()))
						|| (programPart.getPredOwner().equals(localUserInfo.getLocalUserName()))
						|| (programPart.getPredOwner().equals(ProgramPartAnalysisClass.DEFAULT_DEFINITION))) {

					boolean placed = false;

					j = 0;
					while ((j < programFunctionsOrdered.size()) && (!placed)) {
						if ((programFunctionsOrdered.get(j) != null) && (programFunctionsOrdered.get(j).size() > 0)) {
							if ((programFunctionsOrdered.get(j).get(0).getPredDefined().equals(programPart.getPredDefined()))
									&& (programFunctionsOrdered.get(j).get(0).getPredNecessary().equals(programPart.getPredNecessary()))) {
								programFunctionsOrdered.get(j).add(programPart);
								placed = true;
							}
						}
						j++;
					}
					if (!placed) {
						ArrayList<ProgramPartAnalysisClass> current = new ArrayList<ProgramPartAnalysisClass>();
						current.add(programPart);
						programFunctionsOrdered.add(current);
						placed = true;
					}
				}
			}
			/*
			 * else { LOG.info("Not a function. programPart.getHead(): " +
			 * programPart.getHead()); String [] lines = programPart.getLines();
			 * String line = ""; for (int k = 0; k< lines.length; k++) { line +=
			 * lines[k] + "\n"; } LOG.info(line); }
			 */
		}

		String tmp = null;
		ProgramPartAnalysisClass function = null;
		programFunctionsOrderedInJavaScript = new String[programFunctionsOrdered.size()];
		i = 0;
		LOG.info("programFunctionsOrdered.size(): " + programFunctionsOrdered.size());
		while (i < programFunctionsOrdered.size()) {
			j = 0;
			LOG.info("programFunctionsOrdered.get(i).size(): " + programFunctionsOrdered.get(i).size());
			while (j < programFunctionsOrdered.get(i).size()) {
				function = programFunctionsOrdered.get(i).get(j);

				if (j == 0) {
					tmp = "addFuzzificationFunctionDefinition('" + function.getPredDefined() + "', '" + function.getPredNecessary() + "', ";
					tmp += "new Array(";
				} else
					tmp += ", ";

				tmp += "new ownerPersonalization('" + function.getPredOwner() + "', " + function.getFunctionInJavaScript() + ")";
				j++;
			}
			tmp += "))"; // End the javascript arrays.
			programFunctionsOrderedInJavaScript[i] = tmp;
			i++;
		}

		return programFunctionsOrderedInJavaScript;
	}

	public void updateProgramFile(LocalUserInfo localUserInfo, String predDefined, String predNecessary, String predOwner,
			String[][] functionDefinition) throws Exception {

		LOG.info("saving fuzzification: " + predDefined + " depending on " + predNecessary + " personalized for username: " + predOwner
				+ " by username: " + localUserInfo.getLocalUserName() + "\n\n");

		// Security issues ("" strings).
		if ("".equals(predDefined))
			throw new Exception("predDefined cannot be empty string.");
		if ("".equals(predNecessary))
			throw new Exception("predNecessary cannot be empty string.");
		if ("".equals(predOwner))
			throw new Exception("predOwner cannot be empty string.");

		// If I'm not the owner I can change only my fuzzification.
		// If I'm the owner I can change mine and the default one, but no other
		// one.
		if ((!localUserInfo.getLocalUserName().equals(programFileInfo.getFileOwner()))
				|| (!predOwner.equals(ProgramPartAnalysisClass.DEFAULT_DEFINITION))) {
			predOwner = localUserInfo.getLocalUserName();
		}

		ProgramPartAnalysisClass programPart = null;

		File file = new File(programFileInfo.getProgramFileFullPath());
		if (file.exists()) {
			String filePathAux = programFileInfo.getProgramFileFullPath();

			// Remove the extension.
			if (filePathAux.endsWith(".pl"))
				filePathAux = filePathAux.substring(0, filePathAux.length() - ".pl".length());

			// Add the new suffix.
			filePathAux = filePathAux + "_backup_" + Dates.getCurrentDate() + ".txt";

			// Rename the original file.
			LOG.info("renaming " + programFileInfo.getProgramFileFullPath() + " into " + filePathAux);
			File fileAux = new File(filePathAux);
			file.renameTo(fileAux);
		}

		// Create a new file.
		LOG.info("creating the file " + programFileInfo.getProgramFileFullPath());
		file.createNewFile();

		ArrayList<ProgramPartAnalysisClass> programPartsAffected = new ArrayList<ProgramPartAnalysisClass>();
		boolean foundFuzzifications = false;
		boolean copiedBackFuzzifications = false;

		FileWriter fw = new FileWriter(file.getAbsoluteFile());
		BufferedWriter bw = new BufferedWriter(fw);

		int j = 0, k = 0;
		boolean updated = false;
		String[] lines = null;

		for (int i = 0; i < programParts.size(); i++) {
			programPart = programParts.get(i);
			// LOG.info("analyzing the program line: " + programPart.getLine());

			if (!copiedBackFuzzifications) {
				if ((programPart.isFunction()) && (programPart.getPredDefined().equals(predDefined))
						&& (programPart.getPredNecessary().equals(predNecessary))) {
					foundFuzzifications = true;
					programPartsAffected.add(programPart);
				} else {
					if (foundFuzzifications) {
						copiedBackFuzzifications = true;

						updated = false;
						for (j = 0; j < programPartsAffected.size(); j++) {
							if (programPartsAffected.get(j).getPredOwner().equals(predOwner)) {
								programPartsAffected.get(j).updateFunction(functionDefinition);
								updated = true;
							}

							lines = programPartsAffected.get(j).getLines();
							for (k = 0; k < lines.length; k++) {
								bw.write(lines[k] + "\n");
							}
						}

						if (!updated) {
							ProgramPartAnalysisClass newProgramPart = new ProgramPartAnalysisClass();
							newProgramPart.setPredDefined(predDefined);
							newProgramPart.setPredNecessary(predNecessary);
							newProgramPart.setPredOwner(predOwner);
							newProgramPart.updateFunction(functionDefinition);

							lines = newProgramPart.getLines();
							for (k = 0; k < lines.length; k++) {
								bw.write(lines[k] + "\n");
							}
						}
					}
				}
			}

			if ((!foundFuzzifications) || (foundFuzzifications && copiedBackFuzzifications)) {
				lines = programPart.getLines();
				for (k = 0; k < lines.length; k++) {
					bw.write(lines[k] + "\n");
				}
			}
		}

		bw.close();

		programFunctionsOrderedInJavaScript = null;
		// At this point we must clear the CiaoPrologConnection queries cache,
		// but introducing the parameter session
		// or the parameter CiaoPrologConnectionClass complicates this
		// innecessarily. We must do it in the servlet
		// that calls us.
	}

	public String[][] getFunctionDefinition(RequestStoreHouse requestStoreHouse) {
		int counter = 0;
		String[][] functionDefinition = null;
		StringBuilder paramsDebug = new StringBuilder();
		paramsDebug.append("Function definition to save: ");

		while ((requestStoreHouse.getRequestParameter("fpx[" + counter + "]") != null)
				&& (!"".equals((requestStoreHouse.getRequestParameter("fpx[" + counter + "]"))))
				&& (requestStoreHouse.getRequestParameter("fpy[" + counter + "]") != null)
				&& (!"".equals((requestStoreHouse.getRequestParameter("fpy[" + counter + "]"))))) {
			counter++;
		}

		if (counter > 0) {
			functionDefinition = new String[counter][2];
			for (int i = 0; i < counter; i++) {
				functionDefinition[i][0] = requestStoreHouse.getRequestParameter("fpx[" + i + "]");
				functionDefinition[i][1] = requestStoreHouse.getRequestParameter("fpy[" + i + "]");
				paramsDebug.append("\n" + functionDefinition[i][0] + " -> " + functionDefinition[i][1] + " ");
			}
		}
		LOG.info(paramsDebug.toString());
		return functionDefinition;
	}
}

/*
 * 
 */

/* EOF */

