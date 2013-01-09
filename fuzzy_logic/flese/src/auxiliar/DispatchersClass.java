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

import CiaoJava.PLStructure;
import CiaoJava.PLVariable;

public class DispatchersClass {
	private static final Log LOG = LogFactory.getLog(DispatchersClass.class);

	private int BUFSIZE = 4096;
	private int maxFileSize = 50000 * 1024;
	private int maxMemSize = 50000 * 1024;
	
	private static String programFilesPath = null;
	private static String plServerPath = null;
	
	private ServletContext servletContext = null;
	private String doMethod = null;
	private HttpServletRequest request = null;
	private HttpServletResponse response = null;
	private HttpSession session = null;
	private LocalUserNameClass localUserName = null;
	private String fileName = null;
	private String fileOwner = null;
	private CiaoPrologConnectionClass connection = null;
	
	public DispatchersClass(ServletContext servletContext, String doMethod, LocalUserNameClass localUserName, HttpServletRequest request, HttpServletResponse response) 
			throws Exception {
		
		this.servletContext = servletContext;
		if (servletContext == null) throw new Exception("servletContext is null.");
		
		this.doMethod = doMethod;
		if (doMethod == null) throw new Exception("doMethod is null.");
		if ((! "doGet".equals(doMethod)) && (! "doPost".equals(doMethod))) throw new Exception("doMethod is not doGet nor doPost.");
		
		this.request = request;
		if (request == null) throw new Exception("request is null.");
		
		// Debugging information.
		LOG.info(ServletsAuxMethodsClass.requestParametersToString(request));
		
		this.response = response;
		if (response == null) throw new Exception("response is null.");

		// Ask for the previously created session.
		session = request.getSession(false);
		if (session == null) throw new Exception("session is null.");

		this.localUserName = localUserName;
		if (localUserName == null) throw new Exception("localUserName is null.");
		
		String [] programFilesValidPaths = {	
				"/home/java-apps/fuzzy-search/", 
				System.getProperty("java.io.tmpdir") + "/java-apps/fuzzy-search/",
				// servlet.getServletContext().getInitParameter("working-folder-fuzzy-search"),
				"/tmp/java-apps/fuzzy-search/"
		};
		
		if (programFilesPath == null) {
			programFilesPath = FilesMgmtClass.returnProgramFilesValidPath(programFilesValidPaths, LOG);
			LOG.info("programFilesPath: " + programFilesPath);
		}
		
		
		String [] plServerValidSubPaths = {	
				"/home/tomcat/ciao-prolog-1.15.0+r14854/ciao/library/javall/plserver",
				"/usr/share/CiaoDE/ciao/library/javall/plserver",
				"/usr/lib/ciao",
				"/usr/share/CiaoDE",
				"/usr", 
				"/opt", 
				"/home", 
				"/"
		};
		
		if (plServerPath == null) {
			plServerPath = FilesMgmtClass.returnPlServerValidPath(plServerValidSubPaths, LOG);
			LOG.info("plServerPath: " + plServerPath);
		}
		
		// Aqui tendriamos que decidir si hay query o nos limitamos a ejecutar la query "fileNameIntrospectionQuery"
		connection = (CiaoPrologConnectionClass) session.getAttribute("connection");
		
		if (connection == null) {
			connection = new CiaoPrologConnectionClass();
		}

	}

	private void testAndInitialize_fileName_and_fileOwner() throws Exception {
		fileName = request.getParameter("fileName");
		if (fileName == null) throw new Exception("fileName is null.");
		request.setAttribute("fileName", fileName);
		
		fileOwner = request.getParameter("fileOwner");
		if (fileOwner == null) throw new Exception("fileOwner is null.");
		request.setAttribute("fileOwner", fileOwner);
	}
	
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////

	/**
	 * Generates the static html page seen by the user.
	 * Still needs some clean up !!!
	 * @throws Exception
	 */
	public void emptyRequest() throws Exception {
		// Forward to the jsp page.
		ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.SignedInAnswer, "", request, response, LOG);
	}
	
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////

	/**
	 * Executes the introspection query and puts the connection object into the connection session attibute,
	 * so the results can be obtained in any jsp page.
	 * @throws Exception
	 */
	public void runProgramIntrospectionQuery(boolean doForward) throws Exception {
		
		testAndInitialize_fileName_and_fileOwner();
		connection.programFileIntrospectionQuery(plServerPath, programFilesPath, fileOwner, fileName);
		/*
		LOG.info("------");
		LOG.info("------");
		LOG.info("--------> testing query !!! <-----------");
		LOG.info("------");
		LOG.info("------");
		connection.testingQuery(fileOwner, fileName);
		*/
		
		// Update the connection object in the session.
		session.removeAttribute("connection"); 
		session.setAttribute("connection", connection);

		if (doForward) {
			// Forward to the jsp page.
			ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.ProgramFileIntrospectionAnswer, "", request, response, LOG);
		}
	}
	
	/**
	 * Executes any generic query and puts the connection object into the connection session attibute,
	 * so the results can be obtained in any jsp page.
	 * @throws Exception
	 */
	public void runProgramQuery() throws Exception {
		
		testAndInitialize_fileName_and_fileOwner();
		runProgramIntrospectionQuery(false);
		
		String formParameters = " --- Parameters Names and Values --- \n";
	    Enumeration<String> paramNames = request.getParameterNames();
	    while(paramNames.hasMoreElements()) {
	    	String paramName = (String)paramNames.nextElement();
	    	String[] paramValues = request.getParameterValues(paramName);
	    	for(int i=0; i<paramValues.length; i++) {
	    		formParameters += "paramName: " + paramName + " paramValue: " + paramValues[i] + " \n";
	    	}
	    }
	    LOG.info(formParameters);
	    
	    if (request.getParameter("queryLinesCounter") == null) {
	    	ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.ExceptionAjaxPage, "", request, response, LOG);
	    }
	    else {
	    	int queryLinesCounter = Integer.parseInt(request.getParameter("queryLinesCounter"));
	    	QueryConversorClass conversor = new QueryConversorClass(connection, localUserName.getLocalUserName());
	    	String msg = "";

	    	// Parameters to be retrieved and saved:
	    	// quantifier0, quantifier1, predicate, rfuzzyComputeOperator, rfuzzyComputeValue, aggregator;

	    	conversor.subqueryEndTestAndSave();
	    	msg += conversor.subqueryRetrieveAndSaveSubpart("selectQueryStartupType", request, QueryConversorClass.initialPredicate);
	    	msg += conversor.subqueryRetrieveAndSaveSubpart("queryLines.selectAggregator", request, QueryConversorClass.aggregator);

	    	for (int i=0; i<queryLinesCounter; i++) {
	    		conversor.subqueryEndTestAndSave();

	    		msg += conversor.subqueryRetrieveAndSaveSubpart("queryLine["+i+"].selectQuantifier_0", request, QueryConversorClass.quantifier0);
	    		msg += conversor.subqueryRetrieveAndSaveSubpart("queryLine["+i+"].selectQuantifier_1", request, QueryConversorClass.quantifier1);
	    		msg += conversor.subqueryRetrieveAndSaveSubpart("queryLine["+i+"].selectPredicate", request, QueryConversorClass.predicate);
	    		msg += conversor.subqueryRetrieveAndSaveSubpart("queryLine["+i+"].selectRfuzzyComputeOperator", request, QueryConversorClass.rfuzzyComputeOperator);
	    		msg += conversor.subqueryRetrieveAndSaveSubpart("queryLine["+i+"].selectRfuzzyComputeValue", request, QueryConversorClass.rfuzzyComputeValue);
	    	}
	    	LOG.info(msg);

	    	conversor.subqueryEndTestAndSave();
	    	PLStructure query = conversor.queryConvert();
	    	PLVariable [] variables = conversor.getListOfVariables();
	    	String [] variablesNames = conversor.getListOfNamesForVariables();

	    	connection.performQuery(plServerPath, query, programFilesPath, fileOwner, fileName, variables, variablesNames);
	    	// performQuery(PLStructure query, String fileOwner, String fileName, PLVariable [] variables)

	    	// Update the connection object in the session.
	    	session.removeAttribute("connection"); 
	    	session.setAttribute("connection", connection);

	    	// Forward to the jsp page.
	    	ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.RunQueryAnswer, "", request, response, LOG);
	    }
	}
	
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	public void filesList() throws Exception {
		Iterator<FileInfoClass> filesListIterator = FilesMgmtClass.returnFilesIterator(programFilesPath, localUserName.getLocalUserName(), LOG);
		request.setAttribute("filesListIterator", filesListIterator);
		// Forward to the jsp page.
		ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.FilesListAnswer, "", request, response, LOG);
	}
	
	public void uploadFile() throws Exception {
		String msg = "Program File has been uploaded.";
		try {
			uploadFileAux();
		} catch (Exception e) {
			msg = "Error: " + e.getMessage();
		}
		request.setAttribute("uploadResult", msg);
		ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.FileUploadAnswer, "", request, response, LOG);
	}
	
	public void uploadFileAux() throws Exception {
		LOG.info("--- uploadFile invocation ---");
		if ((doMethod == null) || ("doGet".equals(doMethod))) {
			throw new ServletException("Uploads are only allowed using http post method.");	
		}
			
		// Check that we have a file upload request
		boolean isMultipart = ServletFileUpload.isMultipartContent(request);
		if( !isMultipart ){
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
		upload.setSizeMax( maxFileSize );


		// Get the path where we are going to upload the file.
		String filesPath = FilesMgmtClass.getFullPath(programFilesPath, localUserName.getLocalUserName(), null, true);

		// Parse the request to get file items.
		List<FileItem> fileItems = CastingsClass.castList(FileItem.class, upload.parseRequest(request));

		// Process the uploaded file items
		Iterator<FileItem> i = fileItems.iterator();

		while ( i.hasNext () ) 
		{
			FileItem fileItem = (FileItem)i.next();
			if ( !fileItem.isFormField () )	
			{
				// Get the uploaded file parameters
				//	            String fieldName = fi.getFieldName();
				String fileName = fileItem.getName();
				if (fileName == null) {
					throw new Exception("The name of the program file to upload is null.");
				}
				if  ("".equals(fileName)) {
					throw new Exception("The name of the program file to upload is an empty string.");
				}
				if (! fileName.endsWith(".pl")) {
					throw new Exception("The name of the program file to upload must have the extension '.pl'.");
				}
				//	ServletsAuxMethodsClass.addMessageToTheUser(request, "Please choose a correct program file. Allowed file extension is \'.pl\'", LOG);

				String fileNameReal = ""; 
				//	            String contentType = fi.getContentType();
				//	            boolean isInMemory = fi.isInMemory();
				//	            long sizeInBytes = fi.getSize();
				// Write the file
				if( fileName.lastIndexOf("\\") >= 0 ){
					fileNameReal = filesPath + fileName.substring( fileName.lastIndexOf("\\"));
				}
				else{
					fileNameReal = filesPath + fileName.substring(fileName.lastIndexOf("\\")+1);
				}
				File file = new File( fileNameReal ) ;
				fileItem.write( file );
			}
		}
	}	

	public void downloadFile() throws Exception {
		
		testAndInitialize_fileName_and_fileOwner();

		String FileNameWithPath = FilesMgmtClass.getFullPath(programFilesPath, fileOwner, fileName, false);
		// request.getParameter("filename");
		String browser_filename = fileName;

		File                f        = new File(FileNameWithPath);
		int                 length   = 0;
		ServletOutputStream op       = response.getOutputStream();
		String              mimetype = servletContext.getMimeType( FileNameWithPath );

		//
		//  Set the response and go!
		//
		//
		response.setContentType( (mimetype != null) ? mimetype : "application/octet-stream" );
		response.setContentLength( (int)f.length() );
		response.setHeader( "Content-Disposition", "attachment; filename=\"" + browser_filename + "\"" );

		//
		//  Stream to the requester.
		//
		byte[] bbuf = new byte[BUFSIZE];
		DataInputStream in = new DataInputStream(new FileInputStream(f));

		while ((in != null) && ((length = in.read(bbuf)) != -1))
		{
			op.write(bbuf,0,length);
		}

		in.close();
		op.flush();
		op.close();
	}

	public void removeFile() throws Exception {
		
		testAndInitialize_fileName_and_fileOwner();
		
		FilesMgmtClass.removeProgramFile(programFilesPath, fileOwner, fileName, localUserName.getLocalUserName());
		ServletsAuxMethodsClass.addMessageToTheUser(request, "The program file "+fileName+" has been removed. ", LOG);
		
	}
	
	public void viewFile() throws Exception {
		
		testAndInitialize_fileName_and_fileOwner();

		String filePath = null;
		if (localUserName.getLocalUserName().equals(fileOwner)) {
			filePath = FilesMgmtClass.getFullPath(programFilesPath, fileOwner, fileName, false);
		}
		request.setAttribute("filePath", filePath);
		ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.FileViewAnswer, "", request, response, LOG);
	}
	
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	public void listProgramFuzzifications () throws Exception {
		
		testAndInitialize_fileName_and_fileOwner();
		
		String filePath = FilesMgmtClass.getFullPath(programFilesPath, fileOwner, fileName, false);
		request.setAttribute("filePath", filePath);
		
		// Forward to the jsp page.
		ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.ListProgramFuzzificationsAnswer, "", request, response, LOG);
	}
	
	public void saveProgramFuzzification () throws Exception {
		
		testAndInitialize_fileName_and_fileOwner();
		
		String filePath = FilesMgmtClass.getFullPath(programFilesPath, fileOwner, fileName, false);
		request.setAttribute("filePath", filePath);
		
		String predDefined = request.getParameter("predDefined");
		if (predDefined == null) throw new Exception("predDefined is null.");
		
		String predNecessary = request.getParameter("predNecessary");
		if (predNecessary == null) throw new Exception("predNecessary is null.");
		
		String predOwner = request.getParameter("predOwner");
		
		
		int counter=0;
		String [] [] params = null;
		String paramsDebug = "Function definition to save: ";
		
		while ( (request.getParameter("fuzzificationBars["+counter+"].fpx") != null) && 
				(request.getParameter("fuzzificationBars["+counter+"].fpy") != null)) {
			counter++;
		}
		
		if (counter>0) { 
			params = new String[counter][2];
			for (int i=0; i<counter; i++) {
				params[i][0] = request.getParameter("fuzzificationBars["+i+"].fpx");
				params[i][1] = request.getParameter("fuzzificationBars["+i+"].fpy");
				paramsDebug += "\n" + params[i][0] + " -> " + params[i][1] + " ";
			}
		}
		
		LOG.info(paramsDebug);
		
		ProgramAnalysisClass programAnalized = new ProgramAnalysisClass(localUserName.getLocalUserName(), fileName, fileOwner, filePath);
		programAnalized.updateProgramFile(predDefined, predNecessary, predOwner, params);
		
		connection.clearCacheInCiaoPrologConnectionClass();
		
		/* This is just to test if the send button produces errors.
		int j = 0;
		while (true) {
			j++;
		}
		*/
		
		// Forward to the jsp page.
		ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.SaveProgramFuzzificationAnswer, "", request, response, LOG);

	}
	
}








/////