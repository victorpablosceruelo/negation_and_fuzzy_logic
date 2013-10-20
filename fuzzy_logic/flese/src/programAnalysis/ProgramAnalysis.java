package programAnalysis;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import storeHouse.CacheStoreHouse;
import storeHouse.CacheStoreHouseCleaner;
import storeHouse.CacheStoreHouseException;
import storeHouse.RequestStoreHouse;
import auxiliar.LocalUserInfo;
import constants.KConstants;
import filesAndPaths.FilesAndPathsException;
import filesAndPaths.ProgramFileInfo;

public class ProgramAnalysis {
	final Log LOG = LogFactory.getLog(ProgramAnalysis.class);

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
	private ProgramAnalysis(ProgramFileInfo programFileInfo) throws Exception {

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

	public static ProgramAnalysis getProgramAnalysisClass(ProgramFileInfo programFileInfo) throws Exception {
		String fullPath = programFileInfo.getProgramFileFullPath();

		Object o = CacheStoreHouse.retrieve(ProgramAnalysis.class, fullPath, fullPath, fullPath);
		ProgramAnalysis object = (ProgramAnalysis) o;
		if (object == null) {
			object = new ProgramAnalysis(programFileInfo);
			object.getProgramFuzzifications();
			CacheStoreHouse.store(ProgramAnalysis.class, fullPath, fullPath, fullPath, object);
		}
		return object;
	}

	public static void clearCacheInstancesFor(ProgramFileInfo programFileInfo) throws FilesAndPathsException, CacheStoreHouseException {
		String fullPath = programFileInfo.getProgramFileFullPath();

		CacheStoreHouse.store(ProgramAnalysis.class, fullPath, fullPath, fullPath, null);
	}

	private ProgramPartAnalysis[][] getProgramFuzzifications() throws Exception {

		LOG.info("getProgramFuzzifications");
		if (programParts == null)
			throw new Exception("programParts is null.");

		// Cache.
		if (programFunctionsOrdered != null) {
			return programFunctionsOrdered;
		}

		ArrayList<HashMap<String, ProgramPartAnalysis>> progrFunctsOrdered = new ArrayList<HashMap<String, ProgramPartAnalysis>>();
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

							progrFunctsOrdered.get(j).put(programPart.getKeyForHashMap(), programPart);
							placed = true;
						}
					}
					j++;
				}
				if (!placed) {
					HashMap<String, ProgramPartAnalysis> current = new HashMap<String, ProgramPartAnalysis>();
					current.put(programPart.getKeyForHashMap(), programPart);
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
			HashMap<String, ProgramPartAnalysis> tmp1 = progrFunctsOrdered.get(i);
			Collection<ProgramPartAnalysis> tmp2 = tmp1.values();
			programFunctionsOrdered[i] = tmp2.toArray(new ProgramPartAnalysis[tmp2.size()]);
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

	public ProgramPartAnalysis[][] getProgramFuzzifications(LocalUserInfo localUserInfo, String predDefined, String predNecessary,
			String mode) throws Exception {

		if (predDefined == null) {
			predDefined = "";
		}

		if (predNecessary == null) {
			predNecessary = "";
		}

		if ((mode == null) || ("".equals(mode))) {
			mode = KConstants.Request.modeBasic;
		}

		// If logged user is not the file owner then edition mode is always
		// basic.
		if (!(programFileInfo.getFileOwner().equals(localUserInfo.getLocalUserName()))) {
			mode = KConstants.Request.modeBasic;
		}

		if (programFunctionsOrdered == null) {
			getProgramFuzzifications();
		}

		ProgramPartAnalysis function = null;
		ArrayList<ArrayList<ProgramPartAnalysis>> filteredResults = new ArrayList<ArrayList<ProgramPartAnalysis>>();
		ArrayList<ProgramPartAnalysis> filteredResult = null;

		for (int i = 0; i < programFunctionsOrdered.length; i++) {
			filteredResult = new ArrayList<ProgramPartAnalysis>();

			for (int j = 0; j < programFunctionsOrdered[i].length; j++) {
				function = programFunctionsOrdered[i][j];

				if ((predDefined.equals("") || (predDefined.equals(function.getPredDefined())))) {
					if ((predNecessary.equals("") || (predNecessary.equals(function.getPredNecessary())))) {
						if (function.getPredOwner().equals(KConstants.Fuzzifications.DEFAULT_DEFINITION)) {
							// The default definition is always retrieved.
							filteredResult.add(function);
						} else {
							if (!(KConstants.Request.modeAdvanced.equals(mode))) {
								if (function.getPredOwner().equals(localUserInfo.getLocalUserName())) {
									// If the mode is not advanced and the
									// logged user
									// is the fuzzification owner, retrieve it
									// too.
									filteredResult.add(function);
								}
							}
						}
					}
				}
			}

			if (filteredResult.size() > 0) {
				filteredResults.add(filteredResult);
			}
		}

		ProgramPartAnalysis[][] results = new ProgramPartAnalysis[filteredResults.size()][];
		for (int i = 0; i < filteredResults.size(); i++) {
			results[i] = filteredResults.get(i).toArray(new ProgramPartAnalysis[filteredResults.get(i).size()]);
		}
		return results;
	}

	public int updateProgramFile(LocalUserInfo localUserInfo, String predDefined, String predNecessary, String mode,
			String[][] functionDefinition) throws Exception {

		LOG.info("saving the fuzzification " + predDefined + " depending on " + predNecessary + " in mode " + mode);

		// Security issues ("" strings).
		if ("".equals(predDefined))
			throw new Exception("predDefined cannot be empty string.");
		if ("".equals(predNecessary))
			throw new Exception("predNecessary cannot be empty string.");
		if ("".equals(mode)) {
			mode = KConstants.Request.modeBasic;
		}

		// If I'm not the owner I can change only my fuzzification.
		// If I'm the owner I can change mine and the default one, but no other
		// one.
		String predOwner = localUserInfo.getLocalUserName();
		if ((localUserInfo.getLocalUserName().equals(programFileInfo.getFileOwner())) && (KConstants.Request.modeAdvanced.equals(mode))) {
			predOwner = KConstants.Fuzzifications.DEFAULT_DEFINITION;
		}

		ProgramPartAnalysis programPart = null;

		if (programFileInfo.existsFile(false)) {
			programFileInfo.remove();
		}
		File file = programFileInfo.createFile(true);

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

		if (copiedBackFuzzifications) {
			programFunctionsOrdered = null;
			CacheStoreHouseCleaner.clean(programFileInfo);
			return 0;
		}
		return -1;
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

