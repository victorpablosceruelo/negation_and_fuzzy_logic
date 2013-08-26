package filesAndPaths;

import java.io.File;
import java.io.FilenameFilter;
import java.util.ArrayList;
import java.util.Iterator;

import org.apache.commons.logging.Log;

import auxiliar.LocalUserInfo;
import auxiliar.LocalUserInfoException;
import filters.OnlyCiaoPrologFilesFilterClass;
import filters.OnlyLocalUserNameFolderFilterClass;
import filters.OnlyNotLocalUserNameFolderFilterClass;

public class FilesMgmt {

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	/**
	 * Removes the program file, but only if its fileOwner is the localUserName.
	 * 
	 * @param fileName
	 *            is the name of the file to remove.
	 * @param fileOwner
	 *            is the owner of the file to be removed, and its relative path.
	 * @param localUserName
	 *            is the name of the user that requests its removal.
	 * @throws LocalUserInfoException
	 * @throws PathsMgmtException
	 * @exception LocalUserNameFixesClassException
	 *                if owner is empty or null.
	 * @exception Exception
	 *                if it cannot be removed.
	 */
	public static void removeProgramFile(String fileOwner, String fileName, String localUserName) throws FilesMgmtException,
			LocalUserInfoException, PathsMgmtException {

		if (fileName == null) {
			throw new FilesMgmtException("fileName is null");
		}
		LocalUserInfo.checkUserNameIsValid(fileOwner);
		LocalUserInfo.checkUserNameIsValid(localUserName);

		Boolean retVal = false;
		if (fileOwner.equals(localUserName)) {
			PathsMgmt pathsMgmt = new PathsMgmt();
			String fullPath = pathsMgmt.getFullPathOfFile(fileOwner, fileName, false);

			File file = new File(fullPath);
			retVal = file.exists();
			if (!retVal) {
				throw new FilesMgmtException("The program file" + fullPath + "does not exist.");
			}
			retVal = file.delete();
			if (!retVal) {
				throw new FilesMgmtException("The program file" + fullPath + "can not be removed.");
			}
		} else {
			throw new FilesMgmtException("You do not own the program file.");
		}
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	/**
	 * Gets an iterator to iterate on the existing program files.
	 * 
	 * @param localUserName
	 *            is the name of the user that is logged in.
	 * @return the program files iterator, null if there are no program files to
	 *         iterate.
	 * @throws LocalUserInfoException
	 * @throws PathsMgmtException
	 * @throws FileInfoException
	 * @throws Exception
	 */
	public static Iterator<ProgramFileInfo> returnFilesIterator(String localUserName, Log LOG) throws FilesMgmtException,
			LocalUserInfoException, PathsMgmtException, FileInfoException {

		LOG.info("localUserName: " + localUserName);
		LocalUserInfo.checkUserNameIsValid(localUserName);

		Iterator<ProgramFileInfo> programFilesIterator = null;
		ArrayList<ProgramFileInfo> programFilesList = listProgramFiles(localUserName, LOG);
		if ((programFilesList != null) && (!programFilesList.isEmpty())) {
			programFilesIterator = programFilesList.iterator();
		}
		return programFilesIterator;
	}

	/**
	 * Gets a list with the existing program files.
	 * 
	 * @param localUserName
	 *            is the name of the user that is logged in.
	 * @return the program files iterator, null if there are no program files to
	 *         iterate.
	 * @throws PathsMgmtException
	 * @throws LocalUserInfoException
	 * @throws FileInfoException
	 * @exception LocalUserNameFixesClassException
	 *                if owner is empty or null.
	 * @exception Exception
	 *                if there is some problem with a subfolder.
	 */
	private static ArrayList<ProgramFileInfo> listProgramFiles(String localUserName, Log LOG) throws FilesMgmtException, PathsMgmtException,
			LocalUserInfoException, FileInfoException {

		LOG.info("localUserName: " + localUserName);

		PathsMgmt pathsMgmt = new PathsMgmt();
		File dir = new File(pathsMgmt.getProgramFilesPath());

		ArrayList<ProgramFileInfo> currentList = new ArrayList<ProgramFileInfo>();

		FilenameFilter filter;
		String[] subDirs;

		// We list first the localUserName program files.
		LocalUserInfo.checkUserNameIsValid(localUserName);
		filter = (FilenameFilter) new OnlyLocalUserNameFolderFilterClass(localUserName);
		subDirs = dir.list(filter);

		if (subDirs != null) {
			for (int i = 0; i < subDirs.length; i++) {
				// Get filename of file or directory
				currentList = listProgramFilesInSubDir(subDirs[i], currentList);
			}
		}

		// We list in second (and last) place the other program files.
		LocalUserInfo.checkUserNameIsValid(localUserName);
		filter = (FilenameFilter) new OnlyNotLocalUserNameFolderFilterClass(localUserName);
		subDirs = dir.list(filter);

		if (subDirs != null) {
			for (int i = 0; i < subDirs.length; i++) {
				// Get filename of file or directory
				currentList = listProgramFilesInSubDir(subDirs[i], currentList);
			}
		}

		return currentList;
	}

	/**
	 * Gets a list with the existing program files.
	 * 
	 * @param subDir
	 *            is the full path of the subdirectory we are listing.
	 * @return the program files iterator, null if there are no program files to
	 *         iterate.
	 * @throws PathsMgmtException
	 * @exception LocalUserNameFixesClassException
	 *                if owner is empty or null.
	 * @exception Exception
	 *                if there is some problem with a subfolder.
	 */
	private static ArrayList<ProgramFileInfo> listProgramFilesInSubDir(String subDir, ArrayList<ProgramFileInfo> currentList)
			throws FilesMgmtException, PathsMgmtException, FileInfoException {

		if ((subDir == null) || ("".equals(subDir))) {
			throw new FilesMgmtException("listProgramFilesInSubDir: subDir cannot be null nor empty string.");
		}

		PathsMgmt pathsMgmt = new PathsMgmt();
		String realPathSubDir = PathsMgmt.concatSubPaths(pathsMgmt.getProgramFilesPath(), subDir);

		File dir = new File(realPathSubDir);
		FilenameFilter filter = (FilenameFilter) new OnlyCiaoPrologFilesFilterClass();
		String[] files = dir.list(filter);

		if (files != null) {
			for (int i = 0; i < files.length; i++) {
				// Get filename of file or directory
				currentList.add(new ProgramFileInfo(subDir, files[i]));
			}
		}
		return currentList;
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

}
