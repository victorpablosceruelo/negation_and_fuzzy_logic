package managers;

import prologConnector.CiaoPrologNormalQuery;
import prologConnector.CiaoPrologProgramIntrospectionQuery;
import prologConnector.CiaoPrologQueryAnswer;
import prologConnector.CiaoPrologTestingQuery;
import prologConnector.ProgramIntrospection;
import storeHouse.RequestStoreHouseException;
import auxiliar.LocalUserInfoException;
import auxiliar.NextStep;
import constants.KConstants;
import constants.KUrls;
import filesAndPaths.FilesAndPathsException;
import filesAndPaths.ProgramFileInfo;

public class QueriesManager extends AbstractManager {

	public QueriesManager() {
		super();
	}

	@Override
	public NextStep getExceptionPage() {
		NextStep nextStep = new NextStep(KConstants.NextStep.forward_to, KUrls.Pages.Exception, "");
		return nextStep;
	}

	@Override
	public void byDefaultMethod() throws Exception {
		selectProgramFile();
	}

	@Override
	public boolean createSessionIfNull() {
		return false;
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public void selectProgramFile() throws FilesAndPathsException, LocalUserInfoException, RequestStoreHouseException {
		ProgramFileInfo[] filesList = FilesManagerAux.list(requestStoreHouse);
		resultsStoreHouse.setFilesList(filesList);

		// Forward to the jsp page.
		setNextStep(new NextStep(KConstants.NextStep.forward_to, KUrls.Queries.SelectProgramFilePage, ""));
	}

	public void selectDatabase() throws Exception {
		CiaoPrologProgramIntrospectionQuery ciaoPrologProgramIntrospectionQuery = CiaoPrologProgramIntrospectionQuery
				.getInstance(requestStoreHouse.getProgramFileInfo());
		ProgramIntrospection programIntrospection = ciaoPrologProgramIntrospectionQuery.getProgramIntrospection();
		resultsStoreHouse.setCiaoPrologProgramIntrospection(programIntrospection);
		setNextStep(new NextStep(KConstants.NextStep.forward_to, KUrls.Queries.SelectDatabasePage, ""));
	}

	public void selectQuery() throws Exception {
		CiaoPrologProgramIntrospectionQuery ciaoPrologProgramIntrospectionQuery = CiaoPrologProgramIntrospectionQuery
				.getInstance(requestStoreHouse.getProgramFileInfo());
		ProgramIntrospection programIntrospection = ciaoPrologProgramIntrospectionQuery.getProgramIntrospection();
		resultsStoreHouse.setCiaoPrologProgramIntrospection(programIntrospection);
		setNextStep(new NextStep(KConstants.NextStep.forward_to, KUrls.Queries.SelectQueryPage, ""));
	}
	
	public void selectQueryAddLine() throws Exception {
		CiaoPrologProgramIntrospectionQuery ciaoPrologProgramIntrospectionQuery = CiaoPrologProgramIntrospectionQuery
				.getInstance(requestStoreHouse.getProgramFileInfo());
		ProgramIntrospection programIntrospection = ciaoPrologProgramIntrospectionQuery.getProgramIntrospection();
		resultsStoreHouse.setCiaoPrologProgramIntrospection(programIntrospection);
		setNextStep(new NextStep(KConstants.NextStep.forward_to, KUrls.Queries.SelectQueryAddLinePage, ""));
	}
	
	public void selectQueryAddAggr() throws Exception {
		CiaoPrologProgramIntrospectionQuery ciaoPrologProgramIntrospectionQuery = CiaoPrologProgramIntrospectionQuery
				.getInstance(requestStoreHouse.getProgramFileInfo());
		ProgramIntrospection programIntrospection = ciaoPrologProgramIntrospectionQuery.getProgramIntrospection();
		resultsStoreHouse.setCiaoPrologProgramIntrospection(programIntrospection);
		setNextStep(new NextStep(KConstants.NextStep.forward_to, KUrls.Queries.SelectQueryAddAggrPage, ""));
	}
	
	public void selectNegation() throws Exception {
		CiaoPrologProgramIntrospectionQuery ciaoPrologProgramIntrospectionQuery = CiaoPrologProgramIntrospectionQuery
				.getInstance(requestStoreHouse.getProgramFileInfo());
		ProgramIntrospection programIntrospection = ciaoPrologProgramIntrospectionQuery.getProgramIntrospection();
		resultsStoreHouse.setCiaoPrologProgramIntrospection(programIntrospection);
		setNextStep(new NextStep(KConstants.NextStep.forward_to, KUrls.Queries.SelectNegationPage, ""));
	}
	
	public void selectQuantifier() throws Exception {
		CiaoPrologProgramIntrospectionQuery ciaoPrologProgramIntrospectionQuery = CiaoPrologProgramIntrospectionQuery
				.getInstance(requestStoreHouse.getProgramFileInfo());
		ProgramIntrospection programIntrospection = ciaoPrologProgramIntrospectionQuery.getProgramIntrospection();
		resultsStoreHouse.setCiaoPrologProgramIntrospection(programIntrospection);
		setNextStep(new NextStep(KConstants.NextStep.forward_to, KUrls.Queries.SelectQuantifierPage, ""));
	}
	
	public void selectOperator() throws Exception {
		CiaoPrologProgramIntrospectionQuery ciaoPrologProgramIntrospectionQuery = CiaoPrologProgramIntrospectionQuery
				.getInstance(requestStoreHouse.getProgramFileInfo());
		ProgramIntrospection programIntrospection = ciaoPrologProgramIntrospectionQuery.getProgramIntrospection();
		resultsStoreHouse.setCiaoPrologProgramIntrospection(programIntrospection);
		setNextStep(new NextStep(KConstants.NextStep.forward_to, KUrls.Queries.SelectOperatorPage, ""));
	}
	
	public void selectValue() throws Exception {
		CiaoPrologProgramIntrospectionQuery ciaoPrologProgramIntrospectionQuery = CiaoPrologProgramIntrospectionQuery
				.getInstance(requestStoreHouse.getProgramFileInfo());
		ProgramIntrospection programIntrospection = ciaoPrologProgramIntrospectionQuery.getProgramIntrospection();
		resultsStoreHouse.setCiaoPrologProgramIntrospection(programIntrospection);
		setNextStep(new NextStep(KConstants.NextStep.forward_to, KUrls.Queries.SelectValuePage, ""));
	}

	public void evaluate() throws Exception {
		CiaoPrologNormalQuery query = CiaoPrologNormalQuery.getInstance(requestStoreHouse);
		CiaoPrologQueryAnswer[] queryAnswers = query.getQueryAnswers();
		resultsStoreHouse.setCiaoPrologQueryAnswers(queryAnswers);
		setNextStep(new NextStep(KConstants.NextStep.forward_to, KUrls.Queries.EvaluatePage, ""));
	}

	public void test() throws Exception {
		CiaoPrologTestingQuery query = CiaoPrologTestingQuery.getInstance(requestStoreHouse.getProgramFileInfo());
		CiaoPrologQueryAnswer[] queryAnswers = query.getQueryAnswers();
		resultsStoreHouse.setCiaoPrologQueryAnswers(queryAnswers);
		setNextStep(new NextStep(KConstants.NextStep.forward_to, KUrls.Queries.EvaluatePage, ""));
	}


}
