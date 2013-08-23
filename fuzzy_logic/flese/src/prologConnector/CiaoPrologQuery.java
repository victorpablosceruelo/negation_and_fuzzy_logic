package prologConnector;

import java.util.ArrayList;

import CiaoJava.PLStructure;
import CiaoJava.PLVariable;
import auxiliar.LocalUserInfoException;
import filesAndPaths.PathsMgmt;
import filesAndPaths.PathsMgmtException;

public class CiaoPrologQuery {

	private PLStructure query = null;
	private String fileOwner = null;
	private String fileName = null;
	private PLVariable[] variables = null;
	private String[] variablesNames = null;

	private ArrayList<AnswerTermInJavaClass[]> queryAnswers = null;

	public CiaoPrologQuery(String fileOwner, String fileName, PLVariable[] variables,
			String[] variablesNames) throws CiaoPrologQueryException {

		if (fileOwner == null)
			throw new CiaoPrologQueryException("fileOwner cannot be null.");
		if (fileName == null)
			throw new CiaoPrologQueryException("fileName cannot be null.");
		if ("".equals(fileOwner))
			throw new CiaoPrologQueryException("fileOwner cannot be empty string.");
		if ("".equals(fileName))
			throw new CiaoPrologQueryException("fileName cannot be empty string.");
		if (variables == null)
			throw new CiaoPrologQueryException("variables cannot be null.");
		if (variablesNames == null)
			throw new CiaoPrologQueryException("variablesNames cannot be null.");

		for (int i = 0; i < variables.length; i++) {
			if (variables[i] == null) {
				throw new CiaoPrologQueryException("variables[" + i + "] is null.");
			}
		}

		for (int i = 0; i < variablesNames.length; i++) {
			if (variablesNames[i] == null) {
				throw new CiaoPrologQueryException("variablesNames[" + i + "] is null.");
			}
		}

		if (variables.length != variablesNames.length) {
			throw new CiaoPrologQueryException("variables and variablesNames have different length.");
		}

		this.fileOwner = fileOwner;
		this.fileName = fileName;
		this.variables = variables;
		this.variablesNames = variablesNames;

		this.queryAnswers = new ArrayList<AnswerTermInJavaClass[]>();
	}

	
	public void setQuery(PLStructure query) throws CiaoPrologQueryException {
		if (query == null)
			throw new CiaoPrologQueryException("query cannot be null.");
		this.query = query;
	}
	
	public PLStructure getQuery() throws CiaoPrologQueryException {
		if (this.query == null)
			throw new CiaoPrologQueryException("query cannot be null.");
		return query;
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

	public String getProgramFileName() throws PathsMgmtException, LocalUserInfoException {
		PathsMgmt pathsMgmt = new PathsMgmt();
		return pathsMgmt.getFullPathOfFile(this.fileOwner, this.fileName, false);
	}
	
	public String getProgramFileFolderName() throws PathsMgmtException, LocalUserInfoException {
		PathsMgmt pathsMgmt = new PathsMgmt();
		return pathsMgmt.getFullPathOfFile(this.fileOwner, "", false);
	}

	public String toString() {
		return this.query.toString();
	}

}
