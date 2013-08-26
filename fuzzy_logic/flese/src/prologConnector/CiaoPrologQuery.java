package prologConnector;

import java.util.ArrayList;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import CiaoJava.PLStructure;
import CiaoJava.PLVariable;
import filesAndPaths.ProgramFileInfo;

public abstract class CiaoPrologQuery implements CiaoPrologQueryInterface {

	final static protected Log LOG = LogFactory.getLog(CiaoPrologQuery.class);

	private PLStructure query = null;
	private ProgramFileInfo programFileInfo = null;

	private PLVariable[] variables = null;

	protected String[] variablesNames = null;
	protected ArrayList<AnswerTermInJavaClass[]> queryAnswers = null;
	protected boolean isProgramIntrospectionQuery = false;

	protected CiaoPrologQuery(ProgramFileInfo programFileInfo) throws CiaoPrologQueryException {

		if (programFileInfo == null)
			throw new CiaoPrologQueryException("programFileInfo cannot be null.");

		this.programFileInfo = programFileInfo;

		this.queryAnswers = new ArrayList<AnswerTermInJavaClass[]>();
	}

	protected void setRealQuery(PLStructure query, PLVariable[] variables, String[] variablesNames) throws CiaoPrologQueryException {
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

	public ProgramFileInfo getProgramFileInfo() {
		return programFileInfo;
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

	public String toString() {
		return this.query.toString();
	}

}
