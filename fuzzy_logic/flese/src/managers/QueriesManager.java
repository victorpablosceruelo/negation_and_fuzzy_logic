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
	public NextStep byDefaultMethod() throws Exception {
		return introspection();
	}

	@Override
	public boolean createSessionIfNull() {
		return false;
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public NextStep introspection() throws Exception {

		CiaoPrologProgramIntrospectionQuery.getInstance(requestStoreHouse.getProgramFileInfo());

		/*
		 * LOG.info("------"); LOG.info("------");
		 * LOG.info("--------> testing query !!! <-----------");
		 * LOG.info("------"); LOG.info("------");
		 * connection.testingQuery(fileOwner, fileName);
		 */

		// Forward to the jsp page.
		NextStep nextStep = new NextStep(KConstants.NextStep.forward_to, KUrls.Queries.IntrospectionPage, "");
		return nextStep;
	}
	
	public NextStep evaluate() throws Exception {

		// CiaoPrologNormalQuery query = 
		CiaoPrologNormalQuery.getInstance(requestStoreHouse);

		// Forward to the jsp page.
		NextStep nextStep = new NextStep(KConstants.NextStep.forward_to, KUrls.Queries.EvaluatePage, "");
		return nextStep;

	}
	
}
