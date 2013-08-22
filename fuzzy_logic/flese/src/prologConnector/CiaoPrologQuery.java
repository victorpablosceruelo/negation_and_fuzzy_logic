package prologConnector;

import java.util.ArrayList;

import filesAndPaths.PathsMgmt;

import CiaoJava.PLException;
import CiaoJava.PLStructure;
import CiaoJava.PLVariable;

public class CiaoPrologQuery {

	private PLStructure query = null;
	private String programFilesPath = null;
	private String fileOwner = null;
	private String fileName = null;
	private PLVariable[] variables = null;
	private String[] variablesNames = null;
	
	private ArrayList<AnswerTermInJavaClass[]> queryAnswers = null;

	public CiaoPrologQuery(PLStructure query, String programFilesPath, String fileOwner, String fileName, PLVariable[] variables,
			String[] variablesNames) throws Exception {

		if (fileOwner == null)
			throw new Exception("fileOwner cannot be null.");
		if (fileName == null)
			throw new Exception("fileName cannot be null.");
		if (query == null)
			throw new PLException("query cannot be null.");
		if ("".equals(fileOwner))
			throw new Exception("fileOwner cannot be empty string.");
		if ("".equals(fileName))
			throw new Exception("fileName cannot be empty string.");
		if (variables == null)
			throw new Exception("variables cannot be null.");
		if (variablesNames == null)
			throw new Exception("variablesNames cannot be null.");

		for (int i = 0; i < variables.length; i++) {
			if (variables[i] == null) {
				throw new PLException("variables[" + i + "] is null.");
			}
		}

		for (int i = 0; i < variablesNames.length; i++) {
			if (variablesNames[i] == null) {
				throw new PLException("variablesNames[" + i + "] is null.");
			}
		}

		if (variables.length != variablesNames.length) {
			throw new PLException("variables and variablesNames have different length.");
		}
		
		this.query = query;
		this.programFilesPath = programFilesPath;
		this.fileOwner = fileOwner;
		this.fileName = fileName;
		this.variables = variables;
		this.variablesNames = variablesNames;

		this.queryAnswers = new ArrayList<AnswerTermInJavaClass[]>();
	}

	public PLStructure getQuery() {
		return query;
	}

	public String getProgramFilesPath() {
		return programFilesPath;
	}

	public String getFileOwner() {
		return fileOwner;
	}

	public String getFileName() {
		return fileName;
	}

	public PLVariable[] getVariables() {
		return variables;
	}

	public String[] getVariablesNames() {
		return variablesNames;
	}
	
	public int getVariablesLength() {
		return variablesNames.length;
	}
	
	
	public void addQueryAnswer(AnswerTermInJavaClass answerTerm[]) {
		this.queryAnswers.add(answerTerm);
	}
	
	public String getProgramFileName() throws Exception {
		PathsMgmt pathsMgmt = new PathsMgmt();
		return pathsMgmt.getFullPathOf(this.fileOwner, this.fileName, false);
	}
	
	public String toString() {
		return this.query.toString();
	}
	
}
