package managers;

import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;

import javax.servlet.ServletOutputStream;

import results.ResultsStoreHouseUtils;
import storeHouse.CacheStoreHouseCleaner;
import storeHouse.RequestStoreHouseException;
import auxiliar.LocalUserInfo;
import auxiliar.LocalUserInfoException;
import auxiliar.NextStep;
import constants.KConstants;
import constants.KUrls;
import filesAndPaths.FileInfoException;
import filesAndPaths.PathsMgmtException;
import filesAndPaths.ProgramFileInfo;

public class FilesManager extends AbstractManager {

	public FilesManager() {
	}

	@Override
	public NextStep getExceptionPage() {
		NextStep nextStep = new NextStep(KConstants.NextStep.forward_to, KUrls.Pages.Exception, "");
		return nextStep;
	}

	@Override
	public void byDefaultMethod() throws Exception {
		list();
	}

	@Override
	public boolean createSessionIfNull() {
		return false;
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public void list() throws PathsMgmtException, LocalUserInfoException, RequestStoreHouseException {
		ProgramFileInfo[] filesList = FilesManagerAux.list(requestStoreHouse);
		ResultsStoreHouseUtils.updateFilesList(requestStoreHouse, filesList);

		// Forward to the jsp page.
		setNextStep(new NextStep(KConstants.NextStep.forward_to, KUrls.Files.ListPage, ""));
	}

	public void uploadFile() throws Exception {
		String msg = "Program File has been uploaded.";
		setNextStep(new NextStep(KConstants.NextStep.forward_to, KUrls.Files.UploadPage, ""));

		try {
			FilesManagerAux.uploadFileAux(requestStoreHouse);
		} catch (Exception e) {
			msg = "Error: " + e.getMessage();
			setNextStep(null);
		}

		ResultsStoreHouseUtils.addMessage(requestStoreHouse, msg);

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

		if (!(localUserInfo.equals(programFileInfo.getFileOwner()))) {
			throw new Exception("Logged user does not own the program file.");
		}

		programFileInfo.remove();
		ResultsStoreHouseUtils.addMessage(requestStoreHouse, "The program file " + programFileInfo.getFileName() + " has been removed. ");

		CacheStoreHouseCleaner.clean(requestStoreHouse);

		setNextStep(new NextStep(KConstants.NextStep.forward_to, KUrls.Files.RemovePage, ""));
	}

	public void viewFile() throws FileInfoException, FilesManagerException, PathsMgmtException, LocalUserInfoException, RequestStoreHouseException {

		ProgramFileInfo programFileInfo = requestStoreHouse.getProgramFileInfo();
		LocalUserInfo localUserInfo = requestStoreHouse.getSession().getLocalUserInfo();

		String[] fileContents = null;
		if (localUserInfo.equals(programFileInfo.getFileOwner())) {
			try {
				fileContents = programFileInfo.getFileContents();
			} catch (PathsMgmtException e) {
				e.printStackTrace();
				throw e;
			} catch (IOException e) {
				e.printStackTrace();
				throw new FilesManagerException(e.getMessage());
			}
			ResultsStoreHouseUtils.updateFileContents(requestStoreHouse, fileContents);
		} else {
			ResultsStoreHouseUtils.addMessage(requestStoreHouse,
					"You are not allowed to see the contents of the file " + programFileInfo.getFileName());
		}

		setNextStep(new NextStep(KConstants.NextStep.forward_to, KUrls.Files.ViewPage, ""));
	}

}
