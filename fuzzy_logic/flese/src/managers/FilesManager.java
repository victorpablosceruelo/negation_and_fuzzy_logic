package managers;

import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.ServletOutputStream;

import org.apache.commons.fileupload.FileItem;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.apache.commons.fileupload.servlet.ServletFileUpload;

import constants.KConstants;
import constants.KUrls;
import filesAndPaths.FilesMgmt;
import filesAndPaths.ProgramFileInfo;
import auxiliar.CastingsClass;
import auxiliar.NextStep;
import auxiliar.ServletsAuxMethodsClass;

public class FilesManager extends AbstractManager {

	public FilesManager() {
	}

	@Override
	public NextStep getExceptionPage() {
		NextStep nextStep = new NextStep(NextStep.Constants.forward_to, KUrls.Pages.Exception, "");
		return nextStep;
	}

	@Override
	public NextStep byDefaultMethod() throws Exception {

		return null;
	}

	@Override
	public boolean createSessionIfNull() {
		return false;
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	
	public void filesList() throws Exception {
		ProgramFileInfo [] programFiles = FilesMgmt.returnFilesIterator(requestStoreHouse);
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

	
}
