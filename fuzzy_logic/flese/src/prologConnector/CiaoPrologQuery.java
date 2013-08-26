package prologConnector;

import java.util.ArrayList;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import CiaoJava.PLStructure;
import CiaoJava.PLVariable;
import auxiliar.LocalUserInfoException;
import filesAndPaths.PathsMgmt;
import filesAndPaths.PathsMgmtException;

public abstract class CiaoPrologQuery {

	final static protected Log LOG = LogFactory.getLog(CiaoPrologQuery.class);
	
	private PLStructure query = null;
	private String fileOwner = null;
	private String fileName = null;
	
	private PLVariable[] variables = null;
	
	protected String[] variablesNames = null;
	protected ArrayList<AnswerTermInJavaClass[]> queryAnswers = null;
	protected boolean isProgramIntrospectionQuery = false;

	public CiaoPrologQuery(String fileOwner, String fileName) throws CiaoPrologQueryException, PathsMgmtException, LocalUserInfoException {

		if (fileOwner == null)
			throw new CiaoPrologQueryException("fileOwner cannot be null.");
		if (fileName == null)
			throw new CiaoPrologQueryException("fileName cannot be null.");
		if ("".equals(fileOwner))
			throw new CiaoPrologQueryException("fileOwner cannot be empty string.");
		if ("".equals(fileName))
			throw new CiaoPrologQueryException("fileName cannot be empty string.");

		this.fileOwner = fileOwner;
		this.fileName = fileName;

		this.queryAnswers = new ArrayList<AnswerTermInJavaClass[]>();
	}

	
	protected void setRealQuery(PLStructure query, PLVariable[] variables,
			String[] variablesNames) throws CiaoPrologQueryException {
		if (query == null)
			throw new CiaoPrologQueryException("query cannot be null.");
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
		
		this.query = query;
		this.variables = variables;
		this.variablesNames = variablesNames;

		LOG.info(query.toString());
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

	public String [] getKeyForCache() {
		if (! isProgramIntrospectionQuery) {
			return new String [] { fileName, fileOwner };
		}
		else {
			return new String [] { fileName, null };
		}
	}
	
	public String toString() {
		return this.query.toString();
	}

}
