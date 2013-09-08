package filesAndPaths;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;

import auxiliar.LocalUserInfo;
import auxiliar.LocalUserInfoException;

public class ProgramFileInfo {

	private String fileName = null;
	private String fileOwner = null;

	public ProgramFileInfo(String fileOwner, String fileName) throws FilesAndPathsException, LocalUserInfoException {

		if (fileOwner == null) {
			throw new FilesAndPathsException("FileInfoClass constructor: fileOwner can not be null.");
		}
		if ("".equals(fileOwner)) {
			throw new FilesAndPathsException("FileInfoClass constructor: fileOwner can not be empty string.");
		}

		if (fileName == null) {
			throw new FilesAndPathsException("FileInfoClass constructor: fileName can not be null.");
		}
		if ("".equals(fileName)) {
			throw new FilesAndPathsException("FileInfoClass constructor: fileName can not be empty string.");
		}

		LocalUserInfo.checkUserNameIsValid(fileOwner);

		this.fileName = fileName;
		this.fileOwner = fileOwner;
	}

	public String getFileName() {
		return fileName;
	}

	public String getFileOwner() {
		return fileOwner;
	}
	
	public String getInfoForUrls() {
		return "&fileName="+getFileName()+"&fileOwner="+getFileOwner();
	}

	public String getProgramFileFullPath() throws FilesAndPathsException {
		return PathsUtils.concatPathsStrings(getProgramFileFolderFullPath(), fileName);
	}

	public String getProgramFileFolderFullPath() throws FilesAndPathsException {
		PathsMgmt pathsMgmt = new PathsMgmt();
		return PathsUtils.concatPathsStrings(pathsMgmt.getProgramFilesPath(), fileOwner);
	}

	public Boolean canDeleteFile(String localUserName) {
		return fileOwner.equals(localUserName);
	}

	public void remove() throws FilesAndPathsException, FilesAndPathsException {
		String fullPath = getProgramFileFolderFullPath();

		File file = new File(fullPath);
		boolean retVal = file.exists();
		if (!retVal) {
			throw new FilesAndPathsException("The program file" + fullPath + "does not exist.");
		}
		retVal = file.delete();
		if (!retVal) {
			throw new FilesAndPathsException("The program file" + fullPath + "can not be removed.");
		}
	}

	public String[] getFileContents() throws FilesAndPathsException, IOException {
		ArrayList<String> contents = new ArrayList<String>();
		BufferedReader reader = new BufferedReader(new FileReader(getProgramFileFullPath()));
		String line;
		while ((line = reader.readLine()) != null) {
			contents.add(line);
		}
		reader.close();
		return contents.toArray(new String[contents.size()]);
	}
}

/* ---- */
/* ---- */
/* ---- */
/* ---- */
/* ---- */
/* ---- */

