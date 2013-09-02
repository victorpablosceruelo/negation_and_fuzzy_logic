package results;

import java.util.ArrayList;

import filesAndPaths.ProgramFileInfo;

public class ResultsStoreHouse {

	ArrayList<String> messages = new ArrayList<String>();
	ProgramFileInfo[] filesList = new ProgramFileInfo[0];
	String [] fileContents = null;
	String [] fuzzificationsList = null;

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

	public void setFileContents(String[] fileContents) {
		this.fileContents = null;
		if (fileContents != null) {
			this.fileContents = fileContents;
		}
	}

	public void setFuzzificationsList(String[] fuzzificationsList) {
		this.fuzzificationsList = null;
		if (fuzzificationsList != null) {
			this.fuzzificationsList = fuzzificationsList;
		}
		
	}

}
