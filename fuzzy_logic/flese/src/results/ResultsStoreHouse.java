package results;

import java.util.ArrayList;

import prologConnector.CiaoPrologProgramIntrospectionQuery;
import filesAndPaths.ProgramFileInfo;

public class ResultsStoreHouse {

	private ArrayList<String> messages = new ArrayList<String>();
	private ProgramFileInfo[] filesList = new ProgramFileInfo[0];
	private String[] fileContents = null;
	private String[] fuzzificationsList = null;
	private CiaoPrologProgramIntrospectionQuery ciaoPrologProgramIntrospectionQuery = null;

	/**
	 * Adds a message to the request session attribute msgs.
	 * 
	 * @param msg
	 *            is the message to be added. Cannot be null.
	 */
	protected void addMessage(String msg) {
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

	public void setCiaoPrologProgramIntrospectionQuery(CiaoPrologProgramIntrospectionQuery ciaoPrologProgramIntrospectionQuery) {
		this.ciaoPrologProgramIntrospectionQuery = null;
		if (ciaoPrologProgramIntrospectionQuery != null) {
			this.ciaoPrologProgramIntrospectionQuery = ciaoPrologProgramIntrospectionQuery;
		}

	}

	public CiaoPrologProgramIntrospectionQuery getCiaoPrologProgramIntrospectionQuery() {
		return this.ciaoPrologProgramIntrospectionQuery;
	}
}
