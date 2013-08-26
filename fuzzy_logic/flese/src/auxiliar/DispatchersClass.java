package auxiliar;

import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;

import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.commons.fileupload.FileItem;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.apache.commons.fileupload.servlet.ServletFileUpload;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import prologConnector.CiaoPrologConnectionClass;
import prologConnector.PlConnectionsPool;

import storeHouse.SessionStoreHouse;
import CiaoJava.PLStructure;
import CiaoJava.PLVariable;
import constants.KConstants;
import constants.KPages;
import conversors.QueryConversorClass;
import filesAndPaths.ProgramFileInfo;
import filesAndPaths.FilesMgmt;

public class DispatchersClass {
	private static final Log LOG = LogFactory.getLog(DispatchersClass.class);
	private SessionStoreHouse sessionStoreHouse = null;
	private CiaoPrologConnectionClass connection = null;

	
	
	public DispatchersClass(SessionStoreHouse sessionStoreHouse)
			throws Exception {
		connection = PlConnectionsPool.getConnection();

	}

	private void testAndInitialize_fileName_and_fileOwner() throws Exception {
		String fileName = sessionStoreHouse.getRequestParameter(KConstants.Request.fileNameParam);
		String fileOwner = sessionStoreHouse.getRequestParameter(KConstants.Request.fileOwnerParam);
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
		NextStep nextStep = new NextStep(NextStep.Constants.forward_to, KPages.SignedInAnswer, "");
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
		NextStep nextStep = new NextStep(NextStep.Constants.forward_to, KPages.UserOptionsAnswer, "");
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
		return introspectionQueryRequest(true);
	}

	/**
	 * Executes the introspection query and puts the connection object into the
	 * connection session attibute, so the results can be obtained in any jsp
	 * page.
	 * 
	 * @throws Exception
	 */
	public NextStep introspectionQueryRequest(boolean doForward) throws Exception {

		testAndInitialize_fileName_and_fileOwner();
		connection.programFileIntrospectionQuery(plServerPath, programFilesPath, fileOwner, fileName);
		/*
		 * LOG.info("------"); LOG.info("------");
		 * LOG.info("--------> testing query !!! <-----------");
		 * LOG.info("------"); LOG.info("------");
		 * connection.testingQuery(fileOwner, fileName);
		 */

		// Update the connection object in the session.
		session.removeAttribute("connection");
		session.setAttribute("connection", connection);

		if (doForward) {
			// Forward to the jsp page.
			NextStep nextStep = new NextStep(NextStep.Constants.forward_to, KConstants.Pages.introspectionQueryAnswer, "");
			return nextStep;
		}
	}

	/**
	 * Executes any generic query and puts the connection object into the
	 * connection session attibute, so the results can be obtained in any jsp
	 * page.
	 * 
	 * @throws Exception
	 */
	public NextStep runQuery() throws Exception {

		testAndInitialize_fileName_and_fileOwner();
		introspectionQueryRequest(false);

		String formParameters = " --- Parameters Names and Values --- \n";
		Enumeration<String> paramNames = request.getParameterNames();
		while (paramNames.hasMoreElements()) {
			String paramName = (String) paramNames.nextElement();
			String[] paramValues = request.getParameterValues(paramName);
			for (int i = 0; i < paramValues.length; i++) {
				formParameters += "paramName: " + paramName + " paramValue: " + paramValues[i] + " \n";
			}
		}
		LOG.info(formParameters);

		if (request.getParameter("queryLinesCounter") == null) {
			NextStep nextStep = new NextStep(NextStep.Constants.forward_to, KConstants.Pages.ExceptionAjaxPage, "");
			return nextStep;
		} else {
			int queryLinesCounter = Integer.parseInt(request.getParameter("queryLinesCounter"));
			QueryConversorClass conversor = new QueryConversorClass(connection, localUserName.getLocalUserName());
			String msg = "";

			// Parameters to be retrieved and saved:
			// quantifier0, quantifier1, predicate, rfuzzyComputeOperator,
			// rfuzzyComputeValue, aggregator;

			conversor.subqueryEndTestAndSave();
			msg += conversor.subqueryRetrieveAndSaveSubpart("selectQueryStartupType", request, QueryConversorClass.initialPredicate);
			msg += conversor.subqueryRetrieveAndSaveSubpart("queryLines.selectAggregator", request, QueryConversorClass.aggregator);

			for (int i = 0; i < queryLinesCounter; i++) {
				conversor.subqueryEndTestAndSave();

				msg += conversor.subqueryRetrieveAndSaveSubpart("queryLine[" + i + "].selectQuantifier_0", request,
						QueryConversorClass.quantifier0);
				msg += conversor.subqueryRetrieveAndSaveSubpart("queryLine[" + i + "].selectQuantifier_1", request,
						QueryConversorClass.quantifier1);
				msg += conversor.subqueryRetrieveAndSaveSubpart("queryLine[" + i + "].selectPredicate", request,
						QueryConversorClass.predicate);
				msg += conversor.subqueryRetrieveAndSaveSubpart("queryLine[" + i + "].selectRfuzzyComputeOperator", request,
						QueryConversorClass.rfuzzyComputeOperator);
				msg += conversor.subqueryRetrieveAndSaveSubpart("queryLine[" + i + "].selectRfuzzyComputeValue", request,
						QueryConversorClass.rfuzzyComputeValue);
			}
			LOG.info(msg);

			conversor.subqueryEndTestAndSave();
			PLStructure query = conversor.queryConvert();
			PLVariable[] variables = conversor.getListOfVariables();
			String[] variablesNames = conversor.getListOfNamesForVariables();

			connection.performQuery(plServerPath, query, programFilesPath, fileOwner, fileName, variables, variablesNames);
			// performQuery(PLStructure query, String fileOwner, String
			// fileName, PLVariable [] variables)

			// Update the connection object in the session.
			session.removeAttribute("connection");
			session.setAttribute("connection", connection);

			// Forward to the jsp page.
			ServletsAuxMethodsClass.forward_to(KConstants.Pages.RunQueryAnswer, "", request, response, LOG);
		}
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public void filesList() throws Exception {
		Iterator<ProgramFileInfo> filesListIterator = FilesMgmt.returnFilesIterator(programFilesPath, localUserName.getLocalUserName(),
				LOG);
		request.setAttribute("filesListIterator", filesListIterator);
		// Forward to the jsp page.
		ServletsAuxMethodsClass.forward_to(KConstants.Pages.FilesListAnswer, "", request, response, LOG);
	}

	public void uploadFile() throws Exception {
		String msg = "Program File has been uploaded.";
		try {
			uploadFileAux();
		} catch (Exception e) {
			msg = "Error: " + e.getMessage();
		}
		LOG.info(msg);
		request.setAttribute("uploadResult", msg);
		ServletsAuxMethodsClass.forward_to(KConstants.Pages.FileUploadAnswer, "", request, response, LOG);
	}

	public void uploadFileAux() throws Exception {
		LOG.info("--- uploadFileAux invocation ---");
		if ((doMethod == null) || ("doGet".equals(doMethod))) {
			throw new ServletException("Uploads are only allowed using http post method.");
		}

		// Check that we have a file upload request
		boolean isMultipart = ServletFileUpload.isMultipartContent(request);
		if (!isMultipart) {
			throw new Exception("We cannot upload because the content of the request is not multipart.");
		}

		DiskFileItemFactory factory = new DiskFileItemFactory();
		// maximum size that will be stored in memory
		factory.setSizeThreshold(maxMemSize);
		// Location to save data that is larger than maxMemSize.
		// factory.setRepository(new File("/tmp/uploads"));
		factory.setRepository(new File(programFilesPath));

		// Create a new file upload handler
		ServletFileUpload upload = new ServletFileUpload(factory);
		// maximum file size to be uploaded.
		upload.setSizeMax(maxFileSize);

		// Get the path where we are going to upload the file.
		String filesPath = FilesMgmt.getFullPath(programFilesPath, localUserName.getLocalUserName(), null, true);
		if ((filesPath == null) || ("".equals(filesPath))) {
			throw new Exception("ERROR: filesPath cannot be null nor empty string.");
		} else {
			if (!(filesPath.endsWith("/")))
				filesPath += "/";
		}

		// Parse the request to get file items.
		List<FileItem> fileItems = CastingsClass.castList(FileItem.class, upload.parseRequest(request));

		// Process the uploaded file items
		// Iterator<FileItem> i = fileItems.iterator();

		// while ( i.hasNext () )
		for (int i = 0; i < fileItems.size(); i++) {
			// FileItem fileItem = (FileItem)i.next();
			FileItem fileItem = fileItems.get(i);
			if (!fileItem.isFormField()) {
				// Get the uploaded file parameters
				// String fieldName = fi.getFieldName();
				String fileName = fileItem.getName();
				if (fileName == null) {
					throw new Exception("The name of the program file to upload is null.");
				}
				if ("".equals(fileName)) {
					throw new Exception("The name of the program file to upload is an empty string.");
				}
				if (!fileName.endsWith(".pl")) {
					throw new Exception("The name of the program file to upload must have the extension '.pl'.");
				}
				// ServletsAuxMethodsClass.addMessageToTheUser(request,
				// "Please choose a correct program file. Allowed file extension is \'.pl\'",
				// LOG);

				// String fileNameReal = "";
				// String contentType = fi.getContentType();
				// boolean isInMemory = fi.isInMemory();
				// long sizeInBytes = fi.getSize();
				// Write the file
				if (fileName.lastIndexOf("\\") >= 0) {
					fileName = filesPath + fileName.substring(fileName.lastIndexOf("\\"));
				} else
					fileName = filesPath + fileName;

				LOG.info("realFileName: " + fileName);
				File file = new File(fileName);
				fileItem.write(file);
			}
		}
	}

	public void downloadFile() throws Exception {

		testAndInitialize_fileName_and_fileOwner();

		String FileNameWithPath = FilesMgmt.getFullPath(programFilesPath, fileOwner, fileName, false);
		// request.getParameter("filename");
		String browser_filename = fileName;

		File f = new File(FileNameWithPath);
		int length = 0;
		ServletOutputStream op = response.getOutputStream();
		String mimetype = servletContext.getMimeType(FileNameWithPath);

		//
		// Set the response and go!
		//
		//
		response.setContentType((mimetype != null) ? mimetype : "application/octet-stream");
		response.setContentLength((int) f.length());
		response.setHeader("Content-Disposition", "attachment; filename=\"" + browser_filename + "\"");

		//
		// Stream to the requester.
		//
		byte[] bbuf = new byte[BUFSIZE];
		DataInputStream in = new DataInputStream(new FileInputStream(f));

		while ((in != null) && ((length = in.read(bbuf)) != -1)) {
			op.write(bbuf, 0, length);
		}

		in.close();
		op.flush();
		op.close();
	}

	public void removeFile() throws Exception {

		testAndInitialize_fileName_and_fileOwner();

		FilesMgmt.removeProgramFile(programFilesPath, fileOwner, fileName, localUserName.getLocalUserName());
		ServletsAuxMethodsClass.addMessageToTheUser(request, "The program file " + fileName + " has been removed. ", LOG);

	}

	public void viewFile() throws Exception {

		testAndInitialize_fileName_and_fileOwner();

		String filePath = null;
		if (localUserName.getLocalUserName().equals(fileOwner)) {
			filePath = FilesMgmt.getFullPath(programFilesPath, fileOwner, fileName, false);
		}
		request.setAttribute("filePath", filePath);
		ServletsAuxMethodsClass.forward_to(KConstants.Pages.FileViewAnswer, "", request, response, LOG);
	}

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