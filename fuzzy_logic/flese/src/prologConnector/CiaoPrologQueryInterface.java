package prologConnector;

import CiaoJava.PLStructure;
import CiaoJava.PLVariable;
import filesAndPaths.ProgramFileInfo;

public interface CiaoPrologQueryInterface {

	public PLStructure getQuery() throws CiaoPrologConnectorException;

	public ProgramFileInfo getProgramFileInfo();

	public PLVariable[] getVariables();

	public String[] getVariablesNames();

	public int getVariablesLength();

	public void addQueryAnswer(CiaoPrologQueryAnswer ciaoPrologQueryAnswer);
	
	public CiaoPrologQueryAnswer[] getQueryAnswers();

}
