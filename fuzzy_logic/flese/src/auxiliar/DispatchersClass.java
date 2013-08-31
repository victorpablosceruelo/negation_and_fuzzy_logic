package auxiliar;

import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.util.Iterator;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.ServletOutputStream;

import org.apache.commons.fileupload.FileItem;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.apache.commons.fileupload.servlet.ServletFileUpload;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import prologConnector.CiaoPrologNormalQuery;
import prologConnector.CiaoPrologProgramIntrospectionQuery;
import storeHouse.RequestStoreHouse;
import CiaoJava.PLStructure;
import CiaoJava.PLVariable;
import constants.KConstants;
import constants.KUrls;
import conversors.ConversorToPrologQuery;
import filesAndPaths.FilesMgmt;
import filesAndPaths.ProgramFileInfo;

public class DispatchersClass {
	private static final Log LOG = LogFactory.getLog(DispatchersClass.class);
	private RequestStoreHouse sessionStoreHouse = null;

	public DispatchersClass(RequestStoreHouse sessionStoreHouse) throws Exception {
		this.sessionStoreHouse = sessionStoreHouse;
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	/**
	 * Generates the static html page seen by the user. Still needs some clean
	 * up !!!
	 * 
	 * @throws Exception
	 */
	public NextStep emptyRequest() throws Exception {
		// Forward to the jsp page.
		NextStep nextStep = new NextStep(NextStep.Constants.forward_to, KUrls.SignedInAnswer, "");
		return nextStep;
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	/**
	 * Returns the page showing the user information.
	 * 
	 * @throws Exception
	 */
	public NextStep userOptionsRequest() throws Exception {
		NextStep nextStep = new NextStep(NextStep.Constants.forward_to, KUrls.UserOptionsAnswer, "");
		return nextStep;
	}

	/**
	 * Executes the introspection query and puts the connection object into the
	 * connection session attibute, so the results can be obtained in any jsp
	 * page.
	 * 
	 * @throws Exception
	 */
	public NextStep introspectionQueryRequest() throws Exception {
		
		CiaoPrologProgramIntrospectionQuery.getInstance(sessionStoreHouse.getProgramFileInfo());

		/*
		 * LOG.info("------"); LOG.info("------");
		 * LOG.info("--------> testing query !!! <-----------");
		 * LOG.info("------"); LOG.info("------");
		 * connection.testingQuery(fileOwner, fileName);
		 */

			// Forward to the jsp page.
			NextStep nextStep = new NextStep(NextStep.Constants.forward_to, KUrls.introspectionQueryAnswer, "");
			return nextStep;
	}


	/**
	 * Executes any generic query and puts the connection object into the
	 * connection session attibute, so the results can be obtained in any jsp
	 * page.
	 * 
	 * @throws Exception
	 */
	public NextStep runQuery() throws Exception {

		// CiaoPrologNormalQuery query = 
		CiaoPrologNormalQuery.getInstance(sessionStoreHouse);

		// Forward to the jsp page.
		NextStep nextStep = new NextStep(NextStep.Constants.forward_to, KUrls.RunQueryAnswer, "");
		return nextStep;

		/*
		if (nextStep == null) {
			nextStep = new NextStep(NextStep.Constants.forward_to, KPages.ExceptionAjaxPage, "");
			return nextStep;
		}*/

	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////


	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public void ListProgramFuzzificationsRequest() throws Exception {

		testAndInitialize_fileName_and_fileOwner();

		String filePath = FilesMgmt.getFullPath(programFilesPath, fileOwner, fileName, false);
		request.setAttribute("filePath", filePath);

		// Forward to the jsp page.
		ServletsAuxMethodsClass.forward_to(KConstants.Pages.ListProgramFuzzificationsAnswer, "", request, response, LOG);
	}

	public void saveProgramFuzzification() throws Exception {

		testAndInitialize_fileName_and_fileOwner();

		String filePath = FilesMgmt.getFullPath(programFilesPath, fileOwner, fileName, false);
		request.setAttribute("filePath", filePath);

		String predDefined = request.getParameter("predDefined");
		if (predDefined == null)
			throw new Exception("predDefined is null.");

		String predNecessary = request.getParameter("predNecessary");
		if (predNecessary == null)
			throw new Exception("predNecessary is null.");

		String predOwner = request.getParameter("predOwner");
		if (predOwner == null)
			throw new Exception("predOwner is null.");

		int counter = 0;
		String[][] params = null;
		String paramsDebug = "Function definition to save: ";

		while ((request.getParameter("fpx[" + counter + "]") != null) && (request.getParameter("fpy[" + counter + "]") != null)) {
			counter++;
		}

		if (counter > 0) {
			params = new String[counter][2];
			for (int i = 0; i < counter; i++) {
				params[i][0] = request.getParameter("fpx[" + i + "]");
				params[i][1] = request.getParameter("fpy[" + i + "]");
				paramsDebug += "\n" + params[i][0] + " -> " + params[i][1] + " ";
			}
		}

		LOG.info(paramsDebug);

		ProgramAnalysisClass programAnalized = new ProgramAnalysisClass(localUserName.getLocalUserName(), fileName, fileOwner, filePath);
		programAnalized.updateProgramFile(predDefined, predNecessary, predOwner, params);

		connection.clearCacheInCiaoPrologConnectionClass();

		/*
		 * This is just to test if the send button produces errors. int j = 0;
		 * while (true) { j++; }
		 */

		// Forward to the jsp page.
		ServletsAuxMethodsClass.forward_to(KConstants.Pages.SaveProgramFuzzificationAnswer, "", request, response, LOG);

	}

}

// ///