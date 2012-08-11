package servlets;

import javax.servlet.ServletException;
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
import auxiliar.ServletsAuxMethodsClass;
import auxiliar.WorkingFolderClass;



@WebServlet("/UploadServlet")
public class UploadFileServlet extends HttpServlet {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	final Log LOG = LogFactory.getLog(UploadFileServlet.class);
	
	
	private boolean isMultipart;
	private int maxFileSize = 50000 * 1024;
	private int maxMemSize = 50000 * 1024;
	private File file ;

	public void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, java.io.IOException {
		uploadFile(request, response);

	}
	public void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, java.io.IOException {

		throw new ServletException("GET method used with " +
				getClass( ).getName( )+": POST method required.");
	} 
	
	public void uploadFile(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, java.io.IOException {
		
		// Ask for the previously created session.
		HttpSession session = request.getSession(false);
				
		if (ServletsAuxMethodsClass.client_session_is_not_authenticated(session)) {
			LOG.info("session is new. showing index page.");
			ServletsAuxMethodsClass.goToAppIndexPage(request, response, LOG);
		}
		else {
		
			// Check that we have a file upload request
			isMultipart = ServletFileUpload.isMultipartContent(request);
			if( !isMultipart ){
				session.setAttribute("msg1", "ERROR. No file uploaded.");
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

				try{ 
					// Working folder
					WorkingFolderClass workingFolder = new WorkingFolderClass();
					// Get the path where we are going to upload the file.
					String filesPath = workingFolder.getUserWorkingFolder((String) session.getAttribute("user_display_name"));

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
							if ((fileName == null) || ("".equals(fileName))) {
								session.setAttribute("msg1", "Please choose a correct fuzzy database file.");
							}
							else {
								String fileNameReal = ""; 
								//	            String contentType = fi.getContentType();
								//	            boolean isInMemory = fi.isInMemory();
								//	            long sizeInBytes = fi.getSize();
								// Write the file
								if( fileName.lastIndexOf("\\") >= 0 ){
									fileNameReal = filesPath + "/" + fileName.substring( fileName.lastIndexOf("\\"));

								}else{
									fileNameReal = filesPath + "/" + fileName.substring(fileName.lastIndexOf("\\")+1);
								}
								file = new File( fileNameReal ) ;
								fileItem.write( file );
								session.setAttribute("msg1", "Uploaded Filename: " + fileName + " to " + fileNameReal);
							}
						}
					}
				}catch(Exception e) {
					LOG.error("Exception thrown: ");
					LOG.error(e);
					e.printStackTrace();
					// System.out.println(e);
				}
			}
			ServletsAuxMethodsClass.goToSearchMenu(request, response, LOG);
		}	
	}
}



