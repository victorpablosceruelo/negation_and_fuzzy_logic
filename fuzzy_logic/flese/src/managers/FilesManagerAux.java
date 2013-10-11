package managers;

import java.io.File;
import java.io.FilenameFilter;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.ServletException;

import org.apache.commons.fileupload.FileItem;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.apache.commons.fileupload.servlet.ServletFileUpload;

import storeHouse.RequestStoreHouse;
import storeHouse.RequestStoreHouseException;
import auxiliar.CastingsClass;
import auxiliar.LocalUserInfo;
import auxiliar.LocalUserInfoException;
import constants.KConstants;
import filesAndPaths.FilesAndPathsException;
import filesAndPaths.PathsMgmt;
import filesAndPaths.PathsUtils;
import filesAndPaths.ProgramFileInfo;
import filters.OnlyCiaoPrologFilesFilterClass;
import filters.OnlyLocalUserNameFolderFilterClass;
import filters.OnlyNotLocalUserNameFolderFilterClass;

public class FilesManagerAux {

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public static ProgramFileInfo[] listMyFiles(RequestStoreHouse requestStoreHouse) throws FilesAndPathsException, LocalUserInfoException,
			RequestStoreHouseException {
		return listAux(requestStoreHouse, false);
	}

	public static ProgramFileInfo[] list(RequestStoreHouse requestStoreHouse) throws FilesAndPathsException, LocalUserInfoException,
			RequestStoreHouseException {
		return listAux(requestStoreHouse, false);
	}

	public static ProgramFileInfo[] listAux(RequestStoreHouse requestStoreHouse, boolean onlyMine) throws FilesAndPathsException,
			LocalUserInfoException, RequestStoreHouseException {

		LocalUserInfo localUserInfo = requestStoreHouse.getSession().getLocalUserInfo();

		PathsMgmt pathsMgmt = new PathsMgmt();
		File dir = new File(pathsMgmt.getProgramFilesPath());

		ArrayList<ProgramFileInfo> currentList = new ArrayList<ProgramFileInfo>();

		FilenameFilter filter;
		String[] subDirs;

		// We list first the localUserName program files.
		filter = (FilenameFilter) new OnlyLocalUserNameFolderFilterClass(localUserInfo.getLocalUserName());
		subDirs = dir.list(filter);

		if (subDirs != null) {
			for (int i = 0; i < subDirs.length; i++) {
				// Get filename of file or directory
				currentList = listProgramFilesInSubDir(subDirs[i], pathsMgmt, currentList);
			}
		}

		if (!onlyMine) {
			// We list in second (and last) place the other program files.
			filter = (FilenameFilter) new OnlyNotLocalUserNameFolderFilterClass(localUserInfo.getLocalUserName());
			subDirs = dir.list(filter);

			if (subDirs != null) {
				for (int i = 0; i < subDirs.length; i++) {
					// Get filename of file or directory
					currentList = listProgramFilesInSubDir(subDirs[i], pathsMgmt, currentList);
				}
			}
		}

		return currentList.toArray(new ProgramFileInfo[currentList.size()]);
	}

	/**
	 * Gets a list with the existing program files.
	 * 
	 * @param subDir
	 *            is the full path of the subdirectory we are listing.
	 * @return the program files list.
	 * @throws FilesAndPathsException
	 * @throws LocalUserInfoException
	 */
	private static ArrayList<ProgramFileInfo> listProgramFilesInSubDir(String subDir, PathsMgmt pathsMgmt,
			ArrayList<ProgramFileInfo> currentList) throws FilesAndPathsException, LocalUserInfoException {

		if ((subDir == null) || ("".equals(subDir))) {
			return currentList;
		}

		String realPathSubDir = PathsUtils.concatPathsStrings(pathsMgmt.getProgramFilesPath(), subDir);

		File dir = new File(realPathSubDir);
		FilenameFilter filter = (FilenameFilter) new OnlyCiaoPrologFilesFilterClass();
		String[] files = dir.list(filter);

		ProgramFileInfo programFileInfo;
		if (files != null) {
			for (int i = 0; i < files.length; i++) {
				try {
					programFileInfo = new ProgramFileInfo(subDir, files[i]);
				} catch (FilesAndPathsException e) {
					e.printStackTrace();
					programFileInfo = null;
				}

				if (programFileInfo != null)
					currentList.add(programFileInfo);
			}
		}
		return currentList;
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public static String uploadFileAux(RequestStoreHouse requestStoreHouse) throws Exception {
		if ((requestStoreHouse.getDoMethod() == null) || ("doGet".equals(requestStoreHouse.getDoMethod()))) {
			throw new ServletException("Uploads are only allowed using http post method.");
		}

		// Check that we have a file upload request
		boolean isMultipart = requestStoreHouse.requestIsMultipartContent();
		if (!isMultipart) {
			throw new Exception("We cannot upload because the content of the request is not multipart.");
		}

		DiskFileItemFactory factory = new DiskFileItemFactory();
		// maximum size that will be stored in memory
		factory.setSizeThreshold(KConstants.Communications.maxMemSize);
		// Location to save data that is larger than maxMemSize.
		// factory.setRepository(new File("/tmp/uploads"));
		PathsMgmt pathsMgmt = new PathsMgmt();
		String repositoryLocation = pathsMgmt.getProgramFilesPath();
		factory.setRepository(new File(repositoryLocation));

		// Create a new file upload handler
		ServletFileUpload upload = new ServletFileUpload(factory);
		// maximum file size to be uploaded.
		upload.setSizeMax(KConstants.Communications.maxFileSize);

		// Get the path where we are going to upload the file.
		LocalUserInfo localUserInfo = requestStoreHouse.getSession().getLocalUserInfo();
		String filesPath = localUserInfo.getLocalUserName();
		if ((filesPath == null) || ("".equals(filesPath))) {
			throw new Exception("ERROR: filesPath cannot be null nor empty string.");
		}

		// Adequate
		if (!(filesPath.endsWith("/"))) {
			filesPath += "/";
		}

		// Parse the request to get file items.
		List<FileItem> fileItems = CastingsClass.castList(FileItem.class, upload.parseRequest(requestStoreHouse.getRequest()));

		StringBuilder fileNames = new StringBuilder();

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
					throw new Exception("The name of the program file to upload must have the .pl extension.");
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

				File file = new File(fileName);
				fileItem.write(file);

				if (i > 0) {
					fileNames.append(", ");
				}
				fileNames.append(fileName);
			}
		}
		return fileNames.toString();
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

}
