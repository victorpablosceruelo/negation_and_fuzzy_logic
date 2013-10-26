package filesAndPaths;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.nio.file.CopyOption;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import storeHouse.CacheStoreHouseCleaner;
import storeHouse.CacheStoreHouseException;
import auxiliar.Dates;
import auxiliar.LocalUserInfo;
import constants.KConstants;

public class ProgramFileInfo {

	final Log LOG = LogFactory.getLog(ProgramFileInfo.class);

	private String fileName = null;
	private String fileOwner = null;

	private String folderFullPath = null;
	private String fileFullPath = null;

	public ProgramFileInfo(String fileOwner, String fileName) throws FilesAndPathsException {

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

		LocalUserInfo.nullOnlyIfUserNameIsValid(fileOwner);

		this.fileName = fileName;
		this.fileOwner = fileOwner;

		PathsMgmt pathsMgmt = new PathsMgmt();
		this.folderFullPath = PathsUtils.concatPathsStrings(pathsMgmt.getProgramFilesPath(), fileOwner);
		this.fileFullPath = PathsUtils.concatPathsStrings(this.folderFullPath, fileName);

	}

	public String getFileName() {
		return fileName;
	}

	public String getFileOwner() {
		return fileOwner;
	}

	public String getInfoForUrls() {
		return "&fileName=" + getFileName() + "&fileOwner=" + getFileOwner();
	}

	public String getProgramFileFolderFullPath() {
		return this.folderFullPath;
	}

	public String getProgramFileFullPath() throws FilesAndPathsException {
		return this.fileFullPath;
	}

	public String getProgramFileBackupFullPath() throws FilesAndPathsException {
		PathsMgmt pathsMgmt = new PathsMgmt();
		String folderPath = PathsUtils.concatPathsStrings(pathsMgmt.getProgramFilesPath(), KConstants.Application.BackupsFolder);

		String tmp1 = PathsUtils.concatPathsStrings(folderPath, Dates.getCurrentDate());
		String tmp2 = tmp1 + "_" + fileOwner + "_" + fileName;
		return tmp2;
	}

	public Boolean canDeleteFile(String localUserName) throws FilesAndPathsException {
		return existsFile(false) && fileOwner.equals(localUserName);
	}

	/**
	 * Tests if the file exists.
	 * 
	 * @param throwExceptionIfNot
	 * @return
	 * @throws FilesAndPathsException
	 */
	public boolean existsFile(boolean throwExceptionIfNot) throws FilesAndPathsException {
		File file = new File(fileFullPath);
		boolean retVal = file.exists();
		if (throwExceptionIfNot && (!retVal)) {
			throw new FilesAndPathsException("The program file " + fileName + " owned by " + fileOwner + " does not exist.");
		}
		return retVal;
	}

	public File createFile(boolean throwExceptionIfExists) throws FilesAndPathsException {
		if (existsFile(false)) {
			if (throwExceptionIfExists) {
				throw new FilesAndPathsException("The program file " + fileName + " owned by " + fileOwner + " already exists.");
			}
		}

		PathsMgmt pathsMgmt = new PathsMgmt();
		pathsMgmt.createFolder(getProgramFileFolderFullPath(), false);

		File file = new File(getProgramFileFullPath());
		try {
			file.createNewFile();
		} catch (IOException e) {
			if ((e.getMessage() != null) && (!"".equals(e.getMessage()))) {
				throw new FilesAndPathsException(e.getMessage());
			} else {
				throw new FilesAndPathsException("Error when trying to create the new file.");
			}
		}
		return file;
	}

	/**
	 * Performs a file backup and removes it.
	 * 
	 * @return
	 * @throws FilesAndPathsException
	 * @throws CacheStoreHouseException
	 */
	public String remove() throws FilesAndPathsException, CacheStoreHouseException {
		if (existsFile(false)) {
			backup();
			removeFileWithoutBackup();

			CacheStoreHouseCleaner.clean(this);

			// "File " + fileName + " has been removed.";
			return "";
		} else {
			return "File " + fileName + " does not exist.";
		}
	}

	private void removeFileWithoutBackup() throws FilesAndPathsException {
		existsFile(true);
		Path target = Paths.get(fileFullPath);
		try {
			java.nio.file.Files.delete(target);
		} catch (IOException e) {
			e.printStackTrace();
			throw new FilesAndPathsException("The program file " + fileName + " owned by " + fileOwner + " can not be removed.");
		}
	}

	public void backup() throws FilesAndPathsException {
		PathsMgmt pathsMgmt = new PathsMgmt();
		pathsMgmt.createFolder(KConstants.Application.BackupsFolder, false);

		String backupFileFullPath = getProgramFileBackupFullPath();
		Path FROM = Paths.get(fileFullPath);
		Path TO = Paths.get(backupFileFullPath);
		// overwrite existing file, if exists
		CopyOption[] options = new CopyOption[] { StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.COPY_ATTRIBUTES };
		try {
			java.nio.file.Files.copy(FROM, TO, options);
		} catch (IOException e) {
			LOG.info("Error on copy. \n FROM: " + fileFullPath + "\n TO: " + backupFileFullPath + "\n");
			e.printStackTrace();
			throw new FilesAndPathsException("Cannot make a backup of program file " + fileName + " owned by " + fileOwner + ".");
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

