package auxiliar;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.util.ArrayList;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import prologConnector.CiaoPrologProgramIntrospectionQuery;
import storeHouse.CacheStoreHouse;
import storeHouse.CacheStoreHouseException;
import storeHouse.RequestStoreHouse;
import filesAndPaths.FilesAndPathsException;
import filesAndPaths.ProgramFileInfo;

public class ProgramAnalysisClass {
	final Log LOG = LogFactory.getLog(ProgramAnalysisClass.class);

	private ProgramFileInfo programFileInfo;
	private ArrayList<ProgramPartAnalysis> programParts = null;
	private ProgramPartAnalysis[][] programFunctionsOrdered = null;

	/**
	 * Analizes the program file pointed by filePath.
	 * 
	 * @param requestStoreHouse
	 * @throws Exception
	 *             when any of the previous is null or empty string.
	 */
	private ProgramAnalysisClass(ProgramFileInfo programFileInfo) throws Exception {

		this.programFileInfo = programFileInfo;

		LOG.info("\n filePath: " + programFileInfo.getProgramFileFullPath());

		programParts = new ArrayList<ProgramPartAnalysis>();

		BufferedReader reader = new BufferedReader(new FileReader(programFileInfo.getProgramFileFullPath()));

		String line;
		ProgramPartAnalysis programPart = null;
		boolean continues = false;

		while ((line = reader.readLine()) != null) {
			while (line != null) {
				if (!continues) {
					programPart = new ProgramPartAnalysis();
				}
				line = programPart.parse(line);
			}
			continues = programPart.partIsIncomplete();
			if (!continues)
				programParts.add(programPart);
		}
		reader.close();

	}

	public static ProgramAnalysisClass getProgramAnalysisClass(ProgramFileInfo programFileInfo) throws Exception {
		String fullPath = programFileInfo.getProgramFileFullPath();
		String key = ProgramAnalysisClass.class.getName();

		Object o = CacheStoreHouse.retrieve(ProgramAnalysisClass.class, fullPath, key, key);
		ProgramAnalysisClass object = (ProgramAnalysisClass) o;
		if (object == null) {
			object = new ProgramAnalysisClass(programFileInfo);
			object.getProgramFuzzifications();
			CacheStoreHouse.store(CiaoPrologProgramIntrospectionQuery.class, fullPath, key, key, object);
		}
		return object;
	}

	public static void clearCacheInstancesFor(ProgramFileInfo programFileInfo) throws FilesAndPathsException, CacheStoreHouseException {
		String fullPath = programFileInfo.getProgramFileFullPath();
		String key = ProgramAnalysisClass.class.getName();
		CacheStoreHouse.store(ProgramAnalysisClass.class, fullPath, key, key, null);
	}

	private ProgramPartAnalysis[][] getProgramFuzzifications() throws Exception {

		LOG.info("getProgramFuzzificationsInJS");
		if (programParts == null)
			throw new Exception("programParts is null.");

		// Cache.
		if (programFunctionsOrdered != null) {
			return programFunctionsOrdered;
		}

		ArrayList<ArrayList<ProgramPartAnalysis>> progrFunctsOrdered = new ArrayList<ArrayList<ProgramPartAnalysis>>();
		ProgramPartAnalysis programPart = null;
		int i = 0, j = 0;

		for (i = 0; i < programParts.size(); i++) {
			programPart = programParts.get(i);
			if (programPart.isFunction()) {
				boolean placed = false;

				j = 0;
				while ((j < progrFunctsOrdered.size()) && (!placed)) {
					if ((progrFunctsOrdered.get(j) != null) && (progrFunctsOrdered.get(j).size() > 0)) {
						if ((progrFunctsOrdered.get(j).get(0).getPredDefined().equals(programPart.getPredDefined()))
								&& (progrFunctsOrdered.get(j).get(0).getPredNecessary().equals(programPart.getPredNecessary()))) {
							progrFunctsOrdered.get(j).add(programPart);
							placed = true;
						}
					}
					j++;
				}
				if (!placed) {
					ArrayList<ProgramPartAnalysis> current = new ArrayList<ProgramPartAnalysis>();
					current.add(programPart);
					progrFunctsOrdered.add(current);
					placed = true;
				}
			}
			/*
			 * else { LOG.info("Not a function. programPart.getHead(): " +
			 * programPart.getHead()); String [] lines = programPart.getLines();
			 * String line = ""; for (int k = 0; k< lines.length; k++) { line +=
			 * lines[k] + "\n"; } LOG.info(line); }
			 */
		}

		programFunctionsOrdered = new ProgramPartAnalysis[progrFunctsOrdered.size()][];

		for (i = 0; i < progrFunctsOrdered.size(); i++) {
			ArrayList<ProgramPartAnalysis> tmp = progrFunctsOrdered.get(i);
			programFunctionsOrdered[i] = tmp.toArray(new ProgramPartAnalysis[tmp.size()]);
		}
		return programFunctionsOrdered;
	}

	public static String[] getProgramFuzzificationsInJS(ProgramPartAnalysis[][] programFunctions) throws Exception {

		// Use the information cached.
		if (programFunctions == null) {
			return new String[0];
		}

		String tmp = null;
		ProgramPartAnalysis function = null;
		String[] programFunctionsInJavaScript = new String[programFunctions.length];

		for (int i = 0; i < programFunctions.length; i++) {
			for (int j = 0; j < programFunctions[i].length; j++) {
				function = programFunctions[i][j];

				if (j == 0) {
					tmp = "addFuzzificationFunctionDefinition('" + function.getPredDefined() + "', '" + function.getPredNecessary() + "', ";
					tmp += "new Array(";
				} else
					tmp += ", ";

				tmp += "new ownerPersonalization('" + function.getPredOwner() + "', " + function.getFunctionInJavaScript() + ")";
			}
			tmp += "))"; // End the javascript arrays.
			programFunctionsInJavaScript[i] = tmp;
		}

		return programFunctionsInJavaScript;
	}

	public ProgramPartAnalysis[][] getProgramFuzzifications(LocalUserInfo localUserInfo) throws Exception {

		if (programFunctionsOrdered == null) {
			getProgramFuzzifications();
		}

		ProgramPartAnalysis function = null;
		ProgramPartAnalysis[][] filteredResult = new ProgramPartAnalysis[programFunctionsOrdered.length][];
		ArrayList<ProgramPartAnalysis> filteredSingleResult = null;

		for (int i = 0; i < programFunctionsOrdered.length; i++) {
			filteredSingleResult = new ArrayList<ProgramPartAnalysis>();

			for (int j = 0; j < programFunctionsOrdered[i].length; j++) {
				function = programFunctionsOrdered[i][j];

				if ((localUserInfo.getLocalUserName().equals(programFileInfo.getFileOwner()))
						|| (function.getPredOwner().equals(localUserInfo.getLocalUserName()))
						|| (function.getPredOwner().equals(ProgramPartAnalysis.DEFAULT_DEFINITION))) {
					filteredSingleResult.add(function);
				}
			}

			filteredResult[i] = filteredSingleResult.toArray(new ProgramPartAnalysis[filteredSingleResult.size()]);
		}
		return filteredResult;
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
				|| (!predOwner.equals(ProgramPartAnalysis.DEFAULT_DEFINITION))) {
			predOwner = localUserInfo.getLocalUserName();
		}

		ProgramPartAnalysis programPart = null;

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

		ArrayList<ProgramPartAnalysis> programPartsAffected = new ArrayList<ProgramPartAnalysis>();
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
							ProgramPartAnalysis newProgramPart = new ProgramPartAnalysis();
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

		programFunctionsOrdered = null;
		clearCacheInstancesFor(this.programFileInfo);
		CiaoPrologProgramIntrospectionQuery.clearCacheInstancesFor(this.programFileInfo);
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

