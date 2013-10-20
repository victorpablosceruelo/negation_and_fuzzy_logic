package results;

import java.util.ArrayList;

import programAnalysis.ProgramPartAnalysis;
import prologConnector.CiaoPrologQueryAnswer;
import prologConnector.ProgramIntrospection;
import filesAndPaths.ProgramFileInfo;

public class ResultsStoreHouse {

	private ArrayList<String> exceptionMessages = new ArrayList<String>();
	private String resultMessage = "";
	private ProgramFileInfo[] filesList = new ProgramFileInfo[0];
	private String[] fileContents = null;
	private ProgramFileInfo programFileInfo = null;
	private ProgramPartAnalysis [][] programPartAnalysis = null;
	private ProgramIntrospection programIntrospection = null;
	private String[] variablesNames = null;
	private CiaoPrologQueryAnswer[] queryAnswers = new CiaoPrologQueryAnswer[0];

	/**
	 * Adds a message to the request session attribute msgs.
	 * 
	 * @param msg
	 *            is the message to be added. Cannot be null.
	 */
	public void addExceptionMessage(String msg) {
		if ((msg != null) && (!"".equals(msg))) {
			exceptionMessages.add(msg);
		}
	}

	public String[] getExceptionMessages() {
		if (exceptionMessages == null) {
			return new String[0];
		}
		return exceptionMessages.toArray(new String[exceptionMessages.size()]);
	}
	
	public void setResultMessage(String msg) {
		resultMessage = msg;
	}

	public String getResultMessage() {
		if (resultMessage == null) {
			return "";
		}
		return resultMessage;
	}

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

	public void setProgramFileInfo(ProgramFileInfo programFileInfo) {
		this.programFileInfo = null;
		if (programFileInfo != null) {
			this.programFileInfo = programFileInfo;
		}
	}

	public ProgramFileInfo getProgramFileInfo() {
		return programFileInfo;
	}
	
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
