package results;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Set;

import ontologies.InterfaceOntologyQuery;
import ontologies.OntologyQueryVarResult;
import auxiliar.Dates;
import auxiliar.RegistryEntry;
import programAnalysis.ProgramPartAnalysis;
import prologConnector.CiaoPrologQueryAnswer;
import prologConnector.ProgramIntrospection;
import filesAndPaths.ProgramFileInfo;

public class ResultsStoreHouse {

	private HashMap<String, String[]> requestParams = null;
	private String exceptionMsg = null;
	private ArrayList<String> resultMessages = null;
	private ProgramFileInfo[] filesList = new ProgramFileInfo[0];
	private String[] fileContents = null;
	private ProgramFileInfo programFileInfo = null;
	private ProgramPartAnalysis[][] programPartAnalysis = null;
	private ProgramIntrospection programIntrospection = null;
	private String[] variablesNames = null;
	private CiaoPrologQueryAnswer[] queryAnswers = new CiaoPrologQueryAnswer[0];
	private ArrayList<InterfaceOntologyQuery> ontologiesQueries = null;
	private HashMap<String, RegistryEntry> registryEntries = null;

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	/**
	 * Adds a message to the request session attribute msgs.
	 * 
	 * @param msg
	 *            is the message to be added. Cannot be null.
	 */
	public void setExceptionMsg(String exceptionMsg) {
		if ((exceptionMsg != null) && (!"".equals(exceptionMsg))) {
			this.exceptionMsg = Dates.getCurrentDate() + " " + exceptionMsg;
		}
	}

	public String getExceptionMsg() {
		return exceptionMsg;
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public void addResultMessage(String msg) {
		if (msg != null) {
			if (resultMessages == null) {
				resultMessages = new ArrayList<String>();
			}
			resultMessages.add(msg);
		}
	}

	public String [] getResultMessages() {
		if (resultMessages == null) {
			return new String[0];
		}
		return resultMessages.toArray(new String[resultMessages.size()]);
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public void setFilesList(ProgramFileInfo[] filesList) {
		this.filesList = null;
		if (filesList != null) {
			this.filesList = filesList;
		}
	}

	public ProgramFileInfo[] getFilesList() {
		if (filesList == null) {
			return new ProgramFileInfo[0];
		}
		return filesList;
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public void setFileContents(String[] fileContents) {
		this.fileContents = null;
		if (fileContents != null) {
			this.fileContents = fileContents;
		}
	}

	public String[] getfileContents() {
		if (fileContents == null) {
			return new String[0];
		}
		return fileContents;
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public void setProgramFileInfo(ProgramFileInfo programFileInfo) {
		this.programFileInfo = null;
		if (programFileInfo != null) {
			this.programFileInfo = programFileInfo;
		}
	}

	public ProgramFileInfo getProgramFileInfo() {
		return programFileInfo;
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public void setProgramPartAnalysis(ProgramPartAnalysis[][] programPartAnalysis) {
		this.programPartAnalysis = null;
		if (programPartAnalysis != null) {
			this.programPartAnalysis = programPartAnalysis;
		}
	}

	public ProgramPartAnalysis[][] getProgramPartAnalysis() {
		if (programPartAnalysis == null) {
			return new ProgramPartAnalysis[0][];
		}
		return programPartAnalysis;
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public void setCiaoPrologQueryAnswers(CiaoPrologQueryAnswer[] queryAnswers) {
		this.queryAnswers = new CiaoPrologQueryAnswer[0];
		if (queryAnswers != null) {
			this.queryAnswers = queryAnswers;
		}
	}

	public CiaoPrologQueryAnswer[] getCiaoPrologQueryAnswers() {
		if (queryAnswers == null) {
			return new CiaoPrologQueryAnswer[0];
		}
		return this.queryAnswers;
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public void setCiaoPrologQueryVariablesNames(String[] variablesNames) {
		this.variablesNames = null;
		if (variablesNames != null) {
			this.variablesNames = variablesNames;
		}
	}

	public String[] getCiaoPrologQueryVariablesNames() {
		if (variablesNames == null) {
			return new String[0];
		}
		return this.variablesNames;
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public void setCiaoPrologProgramIntrospection(ProgramIntrospection programIntrospection) {
		this.programIntrospection = null;
		if (programIntrospection != null) {
			this.programIntrospection = programIntrospection;
		}
	}

	public ProgramIntrospection getCiaoPrologProgramIntrospection() {
		return this.programIntrospection;
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public void setRequestParamsHashMap(HashMap<String, String[]> requestParams) {
		this.requestParams = requestParams;
	}

	public HashMap<String, String[]> getRequestParamsHashMap() {
		return this.requestParams;
	}
	
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public void resetOntologyQueryResults() {
		this.ontologiesQueries = null;
	}
	
	public void addOntologyQueryResults(InterfaceOntologyQuery ontologyQuery) {
		if (this.ontologiesQueries == null) {
			this.ontologiesQueries = new ArrayList<InterfaceOntologyQuery>();
		}
		this.ontologiesQueries.add(ontologyQuery);
	}
	
	public InterfaceOntologyQuery[] getOntologyQueryResults() {
		if (this.ontologiesQueries == null) {
			return new InterfaceOntologyQuery[0];
		}
		return this.ontologiesQueries.toArray(new InterfaceOntologyQuery[this.ontologiesQueries.size()]);
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public void resetRegistryEntries() {
		this.registryEntries = null;
	}
	
	public void addRegistryEntry(RegistryEntry registryEntry) {
		if (this.registryEntries == null) {
			this.registryEntries = new HashMap<String, RegistryEntry>();
		}
		this.registryEntries.put(registryEntry.getDate(), registryEntry);
	}
	
	public String getRegistryEntries() {
		if (this.registryEntries == null) {
			return "";
		}
		
		Set<String> keysSet = this.registryEntries.keySet();
		String [] keys = keysSet.toArray(new String[keysSet.size()]);
		Arrays.sort(keys, Collections.reverseOrder());
		
		StringBuilder registryEntriesSB = new StringBuilder(); 
		registryEntriesSB.append("<tr>");
		registryEntriesSB.append("<th>");
		registryEntriesSB.append("Time");
		registryEntriesSB.append("</th>");
		registryEntriesSB.append("<th>");
		registryEntriesSB.append("Manager");
		registryEntriesSB.append("</th>");
		registryEntriesSB.append("<th>");
		registryEntriesSB.append("Operation");
		registryEntriesSB.append("</th>");
		registryEntriesSB.append("<th>");
		registryEntriesSB.append("Info");
		registryEntriesSB.append("</th>");
		registryEntriesSB.append("</tr>");
		
		for (int i=0; i<keys.length; i++) {
			RegistryEntry registryEntry = this.registryEntries.get(keys[i]);
			registryEntriesSB.append("<tr>");
			registryEntriesSB.append("<td>");
			registryEntriesSB.append(registryEntry.getDate());
			registryEntriesSB.append("</td>");
			registryEntriesSB.append("<td>");
			registryEntriesSB.append(registryEntry.getManager());
			registryEntriesSB.append("</td>");
			registryEntriesSB.append("<td>");
			registryEntriesSB.append(registryEntry.getOp());
			registryEntriesSB.append("</td>");
			registryEntriesSB.append("<td>");
			registryEntriesSB.append(registryEntry.getMsg());
			registryEntriesSB.append("</td>");
			registryEntriesSB.append("</tr>");
		}
		return registryEntriesSB.toString();
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

}


//EOF
