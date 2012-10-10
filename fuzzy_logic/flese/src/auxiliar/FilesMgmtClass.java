package auxiliar;

import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.ServletOutputStream;
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

public class FilesMgmtClass {

	private int BUFSIZE = 4096;
	final Log LOG = LogFactory.getLog(FilesMgmtClass.class);
	
	private ServletContext servletContext = null;
	private FoldersUtilsClass FoldersUtilsObject;
	private int maxFileSize = 50000 * 1024;
	private int maxMemSize = 50000 * 1024;

	public FilesMgmtClass(ServletContext servletContext) throws FoldersUtilsClassException {
		FoldersUtilsObject = new FoldersUtilsClass();
		this.servletContext = servletContext;
	}
	
	public void uploadFile(String doMethod, HttpSession session, HttpServletRequest request, HttpServletResponse response)
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

	public void downloadFile(String doMethod, HttpSession session, HttpServletRequest request, HttpServletResponse response)
			throws IOException, FoldersUtilsClassException, LocalUserNameFixesClassException {
		LOG.info("--- downloadFile invocation ---");
		
		String fileName = request.getParameter("fileName");
		String fileOwner = request.getParameter("fileOwner");

		String FileNameWithPath = FoldersUtilsObject.getCompletePathOfProgramFile(fileOwner, fileName);
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

	public void removeFile(String doMethod, HttpSession session, HttpServletRequest request, HttpServletResponse response) 
			throws ServletException, IOException {
		LOG.info("--- removeFile invocation ---");

		String fileName = request.getParameter("fileName");
		String fileOwner = (String) request.getParameter("fileOwner");
		String localUserName = (String) session.getAttribute("localUserName");
		
		if ((fileName != null) && (fileOwner != null) && (localUserName != null)) {
		
			try {
				FoldersUtilsObject.removeDataBase(fileName, fileOwner, localUserName);
				ServletsAuxMethodsClass.addMessageToTheUser(request, "The database "+fileName+" has been removed. ", LOG);
			} catch (Exception e) {
				LOG.info("Exception: " + e +": " + e.getMessage());
				e.printStackTrace();
				ServletsAuxMethodsClass.addMessageToTheUser(request, "The database "+fileName+" could not be removed. ", LOG);
			}
		}
		else {
			ServletsAuxMethodsClass.addMessageToTheUser(request, "Sorry. Unknown request. database: "+fileName+" owner: "+fileOwner+" localUserName: "+localUserName, LOG);
		}
	}
	
	public void viewFile(String doMethod, HttpSession session, HttpServletRequest request, HttpServletResponse response) 
			throws FoldersUtilsClassException, LocalUserNameFixesClassException, ServletException, IOException {
		String fileName = request.getParameter("fileName");
		String fileOwner = request.getParameter("fileOwner");

		String filePath = FoldersUtilsObject.getCompletePathOfProgramFile(fileOwner, fileName);
		request.setAttribute("fileName", fileName);
		request.setAttribute("filePath", filePath);
		ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.FilesMgmtFileViewPage, request, response, LOG);
	}
}



