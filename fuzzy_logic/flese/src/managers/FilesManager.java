package managers;

import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;

import javax.servlet.ServletOutputStream;

import storeHouse.CacheStoreHouseCleaner;
import auxiliar.LocalUserInfo;
import auxiliar.NextStep;
import constants.KConstants;
import constants.KUrls;
import filesAndPaths.FilesAndPathsException;
import filesAndPaths.ProgramFileInfo;

public class FilesManager extends AbstractManager {

	public FilesManager() {
	}

	@Override
	public void byDefaultMethod() throws Exception {
		list();
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public void list() throws Exception {
		ProgramFileInfo[] filesList = FilesManagerAux.listMyFiles(requestStoreHouse);
		resultsStoreHouse.setFilesList(filesList);

		// Forward to the jsp page.
		setNextStep(new NextStep(KConstants.NextStep.forward_to, KUrls.Files.ListPage, ""));
	}

	public void uploadDiv() throws Exception {
		setNextStep(new NextStep(KConstants.NextStep.forward_to, KUrls.Files.UploadDivPage, ""));
	}

	public void upload() throws Exception {
		try {
			FilesManagerAux.uploadFileAux(requestStoreHouse);
		} catch (Exception e) {
			resultsStoreHouse.addMessage(e.getMessage());
		}
		setNextStep(new NextStep(KConstants.NextStep.forward_to, KUrls.Files.UploadPage, ""));
	}

	public void download() throws Exception {

		ProgramFileInfo programFileInfo = requestStoreHouse.getProgramFileInfo();
		// request.getParameter("filename");
		String browser_filename = programFileInfo.getProgramFileFullPath();

		File f = new File(programFileInfo.getProgramFileFullPath());
		int length = 0;
		ServletOutputStream op = requestStoreHouse.getResponse().getOutputStream();
		String mimetype = requestStoreHouse.getServletContext().getMimeType(programFileInfo.getProgramFileFullPath());

		//
		// Set the response and go!
		//
		//
		requestStoreHouse.getResponse().setContentType((mimetype != null) ? mimetype : "application/octet-stream");
		requestStoreHouse.getResponse().setContentLength((int) f.length());
		requestStoreHouse.getResponse().setHeader("Content-Disposition", "attachment; filename=\"" + browser_filename + "\"");

		//
		// Stream to the requester.
		//
		byte[] bbuf = new byte[KConstants.Communications.BUFSIZE];
		DataInputStream in = new DataInputStream(new FileInputStream(f));

		while ((in != null) && ((length = in.read(bbuf)) != -1)) {
			op.write(bbuf, 0, length);
		}

		in.close();
		op.flush();
		op.close();

		// We write the destiny. Do not look for a Next Step.
		setNextStep(null);
	}

	public void remove() throws Exception {

		ProgramFileInfo programFileInfo = requestStoreHouse.getProgramFileInfo();
		LocalUserInfo localUserInfo = requestStoreHouse.getSession().getLocalUserInfo();

		if (!(localUserInfo.getLocalUserName().equals(programFileInfo.getFileOwner()))) {
			resultsStoreHouse.addMessage("Logged user does not own the program file.");
		} else {
			String result = programFileInfo.remove();
			if ((result != null) && (result.length() > 0)) {
				resultsStoreHouse.addMessage(result);
			}
			CacheStoreHouseCleaner.clean(requestStoreHouse);
		}

		setNextStep(new NextStep(KConstants.NextStep.forward_to, KUrls.Files.RemovePage, ""));
	}

	public void view() throws Exception {

		ProgramFileInfo programFileInfo = requestStoreHouse.getProgramFileInfo();
		LocalUserInfo localUserInfo = requestStoreHouse.getSession().getLocalUserInfo();

		String[] fileContents = null;
		if (localUserInfo.getLocalUserName().equals(programFileInfo.getFileOwner())) {
			try {
				fileContents = programFileInfo.getFileContents();
			} catch (FilesAndPathsException e) {
				e.printStackTrace();
				throw e;
			} catch (IOException e) {
				e.printStackTrace();
				throw new FilesManagerException(e.getMessage());
			}
			resultsStoreHouse.setFileContents(fileContents);
		} else {
			resultsStoreHouse.addMessage("You are not allowed to see the contents of the file " + programFileInfo.getFileName());
		}

		setNextStep(new NextStep(KConstants.NextStep.forward_to, KUrls.Files.ViewPage, ""));
	}

}
