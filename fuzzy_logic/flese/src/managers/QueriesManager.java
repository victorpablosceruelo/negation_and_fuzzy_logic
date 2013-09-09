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
		buildQuery();
	}

	@Override
	public boolean createSessionIfNull() {
		return false;
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public void buildQuery() throws Exception {
		CiaoPrologProgramIntrospectionQuery ciaoPrologProgramIntrospectionQuery = CiaoPrologProgramIntrospectionQuery
				.getInstance(requestStoreHouse.getProgramFileInfo());
		ProgramIntrospection programIntrospection = ciaoPrologProgramIntrospectionQuery.getProgramIntrospection();
		resultsStoreHouse.setCiaoPrologProgramIntrospection(programIntrospection);
		setNextStep(new NextStep(KConstants.NextStep.forward_to, KUrls.Queries.BuildQueryPage, ""));
	}

	public void addLineToQuery() throws Exception {
		CiaoPrologProgramIntrospectionQuery ciaoPrologProgramIntrospectionQuery = CiaoPrologProgramIntrospectionQuery
				.getInstance(requestStoreHouse.getProgramFileInfo());
		CiaoPrologQueryAnswer[] queryAnswers = ciaoPrologProgramIntrospectionQuery.getQueryAnswers();
		resultsStoreHouse.setCiaoPrologQueryAnswers(queryAnswers);
		setNextStep(new NextStep(KConstants.NextStep.forward_to, KUrls.Queries.AddLineToQueryPage, ""));
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

	public void listDBs() throws FilesAndPathsException, LocalUserInfoException, RequestStoreHouseException {
		ProgramFileInfo[] filesList = FilesManagerAux.list(requestStoreHouse);
		resultsStoreHouse.setFilesList(filesList);

		// Forward to the jsp page.
		setNextStep(new NextStep(KConstants.NextStep.forward_to, KUrls.Queries.ListDBsPage, ""));
	}

}
