package servlets;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
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



@WebServlet("/UploadServlet")
public class UploadFileServlet extends HttpServlet {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	final Log LOG = LogFactory.getLog(UploadFileServlet.class);
	private static String filesPath = "";
	
	private boolean isMultipart;
	private int maxFileSize = 50000 * 1024;
	private int maxMemSize = 50000 * 1024;
	private File file ;

	public void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, java.io.IOException {
		// Check that we have a file upload request
		isMultipart = ServletFileUpload.isMultipartContent(request);
		response.setContentType("text/html");
		java.io.PrintWriter out = response.getWriter( );
		if( !isMultipart ){
			out.println("<html>");
			out.println("<head>");
			out.println("<title>Servlet upload</title>");  
			out.println("</head>");
			out.println("<body>");
			out.println("<p>No file uploaded</p>"); 
			out.println("</body>");
			out.println("</html>");
			return;
		}
		
		DiskFileItemFactory factory = new DiskFileItemFactory();
		// maximum size that will be stored in memory
		factory.setSizeThreshold(maxMemSize);
		// Location to save data that is larger than maxMemSize.
		factory.setRepository(new File("/tmp/uploads"));

		// Create a new file upload handler
		ServletFileUpload upload = new ServletFileUpload(factory);
		// maximum file size to be uploaded.
		upload.setSizeMax( maxFileSize );
		
		if ((filesPath==null) || (filesPath.equals(""))) {
			whereShouldBeUploadedTheFile();
		}

		try{ 
			// Parse the request to get file items.
			List<FileItem> fileItems = CastingsClass.castList(FileItem.class, upload.parseRequest(request));

			// Process the uploaded file items
			Iterator<FileItem> i = fileItems.iterator();

			out.println("<html>");
			out.println("<head>");
			out.println("<title>Servlet upload</title>");  
			out.println("</head>");
			out.println("<body>");
			while ( i.hasNext () ) 
			{
				FileItem fileItem = (FileItem)i.next();
				if ( !fileItem.isFormField () )	
				{
					// Get the uploaded file parameters
					//	            String fieldName = fi.getFieldName();
					String fileName = fileItem.getName();
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
					file = new File( fileNameReal ) ;
					fileItem.write( file ) ;
					out.println("Uploaded Filename: " + fileName + "<br>");
					out.println("Destiny Filename: " + fileNameReal + "<br>");
				}
			}
			out.println("</body>");
			out.println("</html>");
		}catch(Exception ex) {
			System.out.println(ex);
		}
	}
	public void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, java.io.IOException {

		throw new ServletException("GET method used with " +
				getClass( ).getName( )+": POST method required.");
	} 
	
	private void whereShouldBeUploadedTheFile() {
		// Try the different options one by one.
		test_files_path("/home/java-apps/fuzzy-search/");
		test_files_path(System.getProperty("java.io.tmpdir") + "/java-apps/fuzzy-search/"); 
		test_files_path(getServletContext().getInitParameter("file-upload"));
		test_files_path("/tmp/java-apps/fuzzy-search/");
		LOG.info("choosen folder for uploads: " + filesPath);
	}
	
	private void test_files_path(String newFilesPath) {
		LOG.info("testFilesPath: testing: " + newFilesPath);
		boolean retval = false;
		String testFolder=newFilesPath+".test";
		if ((filesPath!=null) && (! filesPath.equals(""))) {
			return; // No need to update its value.
		}
		if ((newFilesPath==null) || (newFilesPath.equals(""))){
			return; // Invalid input value
		}
			
		File dir; 
		try {
			dir = new File(testFolder);
			if (dir.exists()) {
				dir.delete();
			}
			retval = dir.mkdirs();
		} 
		catch (Exception ex) {
			LOG.info("testFilesPath: not valid: " + newFilesPath);
			LOG.info("Exception: " + ex);
		}
		if (retval) {
			filesPath = newFilesPath;
		}
	}

	
}



