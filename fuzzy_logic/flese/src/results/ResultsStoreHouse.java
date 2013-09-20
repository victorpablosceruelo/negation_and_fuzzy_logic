package results;

import java.util.ArrayList;

import prologConnector.CiaoPrologQueryAnswer;
import prologConnector.ProgramIntrospection;
import filesAndPaths.ProgramFileInfo;

public class ResultsStoreHouse {

	private ArrayList<String> messages = new ArrayList<String>();
	private ProgramFileInfo[] filesList = new ProgramFileInfo[0];
	private String[] fileContents = null;
	private String[] fuzzificationsList = null;
	private ProgramIntrospection programIntrospection = null;
	private String [] variablesNames = null;
	private CiaoPrologQueryAnswer[] queryAnswers = new CiaoPrologQueryAnswer[0];

	/**
	 * Adds a message to the request session attribute msgs.
	 * 
	 * @param msg
	 *            is the message to be added. Cannot be null.
	 */
	public void addMessage(String msg) {
		if ((msg != null) && (!"".equals(msg))) {
			messages.add(msg);
		}
	}

	public void setFilesList(ProgramFileInfo[] filesList) {
		this.filesList = null;
		if (filesList != null) {
			this.filesList = filesList;
		}
	}

	public ProgramFileInfo[] getFilesList() {
		return filesList;
	}

	public void setFileContents(String[] fileContents) {
		this.fileContents = null;
		if (fileContents != null) {
			this.fileContents = fileContents;
		}
	}

	public String[] getfileContents() {
		return fileContents;
	}

	public void setFuzzificationsList(String[] fuzzificationsList) {
		this.fuzzificationsList = null;
		if (fuzzificationsList != null) {
			this.fuzzificationsList = fuzzificationsList;
		}
	}

	public String[] getFuzzificationsList() {
		return fuzzificationsList;
	}

	public void setCiaoPrologQueryAnswers(CiaoPrologQueryAnswer[] queryAnswers) {
		this.queryAnswers = new CiaoPrologQueryAnswer[0];
		if (queryAnswers != null) {
			this.queryAnswers = queryAnswers;
		}
	}

	public CiaoPrologQueryAnswer[] getCiaoPrologQueryAnswers() {
		return this.queryAnswers;
	}
	
	public void setCiaoPrologQueryVariablesNames(String [] variablesNames) {
		this.variablesNames = null;
		if (variablesNames != null) {
			this.variablesNames = variablesNames;
		}
	}
	
	public String [] getCiaoPrologQueryVariablesNames() {
		return this.variablesNames;
	}
	
	public void setCiaoPrologProgramIntrospection(ProgramIntrospection programIntrospection) {
		this.programIntrospection = null;
		if (programIntrospection != null) {
			this.programIntrospection = programIntrospection;
		}
	}

	public ProgramIntrospection getCiaoPrologProgramIntrospection() {
		return this.programIntrospection;
	}
}
