package managers;

import prologConnector.CiaoPrologNormalQuery;
import prologConnector.CiaoPrologProgramIntrospectionQuery;
import auxiliar.NextStep;
import constants.KConstants;
import constants.KUrls;

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

		CiaoPrologProgramIntrospectionQuery ciaoPrologProgramIntrospectionQuery = CiaoPrologProgramIntrospectionQuery.getInstance(requestStoreHouse.getProgramFileInfo());
		requestStoreHouse.getResultsStoreHouse().setCiaoPrologProgramIntrospectionQuery(ciaoPrologProgramIntrospectionQuery);
		
		/*
		 * LOG.info("------"); LOG.info("------");
		 * LOG.info("--------> testing query !!! <-----------");
		 * LOG.info("------"); LOG.info("------");
		 * connection.testingQuery(fileOwner, fileName);
		 */

		// Forward to the jsp page.
		setNextStep(new NextStep(KConstants.NextStep.forward_to, KUrls.Queries.BuildQueryPage, ""));
	}
	
	public void evaluate() throws Exception {

		// CiaoPrologNormalQuery query = 
		CiaoPrologNormalQuery.getInstance(requestStoreHouse);

		// Forward to the jsp page.
		setNextStep(new NextStep(KConstants.NextStep.forward_to, KUrls.Queries.EvaluatePage, ""));
	}
	
}
