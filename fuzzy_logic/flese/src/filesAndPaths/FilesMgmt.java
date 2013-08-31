package filesAndPaths;

import java.io.File;
import java.io.FilenameFilter;
import java.util.ArrayList;

import storeHouse.RequestStoreHouse;
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
	 * Gets a list with the existing program files.
	 * 
	 * @param localUserName
	 *            is the name of the user that is logged in.
	 * @return the program files iterator, null if there are no program files to
	 *         iterate.
	 * @throws PathsMgmtException
	 */
	public static ProgramFileInfo[] list(RequestStoreHouse requestStoreHouse) throws PathsMgmtException {

		LocalUserInfo localUserInfo = requestStoreHouse.session.getLocalUserInfo();

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

		// We list in second (and last) place the other program files.
		filter = (FilenameFilter) new OnlyNotLocalUserNameFolderFilterClass(localUserInfo.getLocalUserName());
		subDirs = dir.list(filter);

		if (subDirs != null) {
			for (int i = 0; i < subDirs.length; i++) {
				// Get filename of file or directory
				currentList = listProgramFilesInSubDir(subDirs[i], pathsMgmt, currentList);
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
	 */
	private static ArrayList<ProgramFileInfo> listProgramFilesInSubDir(String subDir, PathsMgmt pathsMgmt,
			ArrayList<ProgramFileInfo> currentList) {

		if ((subDir == null) || ("".equals(subDir))) {
			return currentList;
		}

		String realPathSubDir = PathsMgmt.concatSubPaths(pathsMgmt.getProgramFilesPath(), subDir);

		File dir = new File(realPathSubDir);
		FilenameFilter filter = (FilenameFilter) new OnlyCiaoPrologFilesFilterClass();
		String[] files = dir.list(filter);

		ProgramFileInfo programFileInfo;
		if (files != null) {
			for (int i = 0; i < files.length; i++) {
				try {
					programFileInfo = new ProgramFileInfo(subDir, files[i]);
				} catch (FileInfoException e) {
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

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

}
