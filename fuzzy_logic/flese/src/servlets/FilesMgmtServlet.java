package servlets;

import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.ServletOutputStream;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import java.io.*;
import java.util.*;

import org.apache.commons.fileupload.FileItem;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.apache.commons.fileupload.servlet.ServletFileUpload;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

// import socialAuth.SocialAuthenticationServlet;
// import org.apache.commons.fileupload.FileUploadException;
// import org.apache.commons.io.output.*;

import auxiliar.CastingsClass;
import auxiliar.FoldersUtilsClassException;
import auxiliar.LocalUserNameFixesClassException;
import auxiliar.ServletsAuxMethodsClass;
import auxiliar.FoldersUtilsClass;


@WebServlet("/FilesMgmtServlet")
public class FilesMgmtServlet extends HttpServlet {

	private int BUFSIZE = 4096;
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	final Log LOG = LogFactory.getLog(FilesMgmtServlet.class);
	
	private FoldersUtilsClass FoldersUtilsObject = null;
	private int maxFileSize = 50000 * 1024;
	private int maxMemSize = 50000 * 1024;

	public void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, java.io.IOException {
		LOG.info("--- doGet invocation ---");
		doGetAndDoPost("doGet", request, response);
		LOG.info("--- doGet end ---");
	} 
	
	public void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, java.io.IOException {
		LOG.info("--- doPost invocation ---");
		doGetAndDoPost("doPost", request, response);
		LOG.info("--- doPost end ---");
	}
	
	private void doGetAndDoPost(String doMethod, HttpServletRequest request, HttpServletResponse response) 
			throws ServletException, IOException {
		// Ask for the previously created session.
		HttpSession session = request.getSession(false);
		
		if (ServletsAuxMethodsClass.clientSessionIsAuthenticated(request, response, LOG)) {
			String request_op = request.getParameter("op");
			LOG.info("op: " + request_op);
			try{
				// Initialize.
				FoldersUtilsObject = new FoldersUtilsClass();

				if (request_op != null) {
					if ("upload".equals(request_op)) {
						uploadFile(doMethod, session, request, response);
					}
					if ("download".equals(request_op)) {
						downloadFile(doMethod, session, request, response);
					}
					if ("remove".equals(request_op)) {
						removeFile(doMethod, session, request, response);
					}
					if ("view".equals(request_op)) {
						String database = request.getParameter("database");
						String owner = request.getParameter("owner");

						String filePath = FoldersUtilsObject.getCompletePathOfDatabase(owner, database);
						request.setAttribute("fileName", database);
						request.setAttribute("filePath", filePath);
						ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.FileView_Page, request, response, LOG);
					}
				}
			}catch(Exception e) {
				LOG.error("Exception thrown: ");
				LOG.error(e);
				e.printStackTrace();
				// System.out.println(e);
			}

			if ((request_op == null) || 
					((! ("upload".equals(request_op))) && (! ("download".equals(request_op))) &&
				     (! ("remove".equals(request_op))) && (! ("view".equals(request_op))))) {
				ServletsAuxMethodsClass.addMessageToTheUser(request, "Strange op in request. op: " + request_op, LOG);
			}

			if ((request_op == null) || ((! ("download".equals(request_op))) && (! ("view".equals(request_op))))) {
				ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.DataBasesMenuServlet_Page, request, response, LOG);
			}
		}
	}
	
	private void uploadFile(String doMethod, HttpSession session, HttpServletRequest request, HttpServletResponse response)
			throws Exception {
		LOG.info("--- uploadFile invocation ---");
		if ((doMethod == null) || ("doGet".equals(doMethod))) {
			throw new ServletException("GET method used with " + getClass( ).getName( )+": POST method required.");	
		}
			
		// Check that we have a file upload request
		boolean isMultipart = ServletFileUpload.isMultipartContent(request);
		if( !isMultipart ){
			ServletsAuxMethodsClass.addMessageToTheUser(request, "ERROR. No file uploaded.", LOG);
		}
		else {

			DiskFileItemFactory factory = new DiskFileItemFactory();
			// maximum size that will be stored in memory
			factory.setSizeThreshold(maxMemSize);
			// Location to save data that is larger than maxMemSize.
			factory.setRepository(new File("/tmp/uploads"));

			// Create a new file upload handler
			ServletFileUpload upload = new ServletFileUpload(factory);
			// maximum file size to be uploaded.
			upload.setSizeMax( maxFileSize );


			// Get the path where we are going to upload the file.
			String localUserName = (String) session.getAttribute("localUserName");
			String filesPath = FoldersUtilsObject.getCompletePathOfOwner(localUserName, true);

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
					if ((fileName == null) || ("".equals(fileName)) || (! fileName.endsWith(".pl"))) {
						ServletsAuxMethodsClass.addMessageToTheUser(request, "Please choose a correct program file. Allowed file extension is \'.pl\'", LOG);
					}
					else {
						String fileNameReal = ""; 
						//	            String contentType = fi.getContentType();
						//	            boolean isInMemory = fi.isInMemory();
						//	            long sizeInBytes = fi.getSize();
						// Write the file
						if( fileName.lastIndexOf("\\") >= 0 ){
							fileNameReal = filesPath + fileName.substring( fileName.lastIndexOf("\\"));

						}else{
							fileNameReal = filesPath + fileName.substring(fileName.lastIndexOf("\\")+1);
						}
						File file = new File( fileNameReal ) ;
						fileItem.write( file );
						ServletsAuxMethodsClass.addMessageToTheUser(request, "Uploaded Filename: " + fileName + " to " + fileNameReal, LOG);
					}
				}
			}

		}
	}	

	private void downloadFile(String doMethod, HttpSession session, HttpServletRequest request, HttpServletResponse response)
			throws IOException, FoldersUtilsClassException, LocalUserNameFixesClassException {
		LOG.info("--- downloadFile invocation ---");
		
		String database = request.getParameter("database");
		String owner = request.getParameter("owner");

		String filename = FoldersUtilsObject.getCompletePathOfDatabase(owner, database);
		// request.getParameter("filename");
		String browser_filename = database;

		File                f        = new File(filename);
		int                 length   = 0;
		ServletOutputStream op       = response.getOutputStream();
		ServletContext      context  = getServletConfig().getServletContext();
		String              mimetype = context.getMimeType( filename );

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

	private void removeFile(String doMethod, HttpSession session, HttpServletRequest request, HttpServletResponse response) 
			throws ServletException, IOException {
		LOG.info("--- removeFile invocation ---");

		String database = request.getParameter("database");
		String owner = (String) request.getParameter("owner");
		String localUserName = (String) session.getAttribute("localUserName");
		
		if ((database != null) && (owner != null) && (localUserName != null)) {
		
			try {
				FoldersUtilsObject.removeDataBase(database, owner, localUserName);
				ServletsAuxMethodsClass.addMessageToTheUser(request, "The database "+database+" has been removed. ", LOG);
			} catch (Exception e) {
				LOG.info("Exception: " + e +": " + e.getMessage());
				e.printStackTrace();
				ServletsAuxMethodsClass.addMessageToTheUser(request, "The database "+database+" could not be removed. ", LOG);
			}
		}
		else {
			ServletsAuxMethodsClass.addMessageToTheUser(request, "Sorry. Unknown request. database: "+database+" owner: "+owner+" localUserName: "+localUserName, LOG);
		}
	}
}



