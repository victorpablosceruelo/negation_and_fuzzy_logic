package uploads;

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
//import org.apache.commons.fileupload.FileUploadException;
//import org.apache.commons.io.output.*;

import auxiliar.StorageClass;
import auxiliar.StorageClass.ValidPaths;
import auxiliar.Castings;



@WebServlet("/UploadServlet")
public class UploadServlet extends HttpServlet {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private boolean isMultipart;
	private String filePath;
	private int maxFileSize = 50000 * 1024;
	private int maxMemSize = 50000 * 1024;
	private File file ;


	
	public void init( ){
		StorageClass aux = new StorageClass(this);
		filePath = aux.getPath(ValidPaths.programs);
	}

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

		try{ 
			// Parse the request to get file items.
			List<FileItem> fileItems = Castings.castList(FileItem.class, upload.parseRequest(request));

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
						fileNameReal = filePath + fileName.substring( fileName.lastIndexOf("\\"));

					}else{
						fileNameReal = filePath + fileName.substring(fileName.lastIndexOf("\\")+1);
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
}



