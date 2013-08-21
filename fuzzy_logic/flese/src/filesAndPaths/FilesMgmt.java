package filesAndPaths;

import java.io.File;
import java.io.FilenameFilter;
import java.util.ArrayList;
import java.util.Iterator;

import org.apache.commons.logging.Log;

import auxiliar.FileInfoClass;
import auxiliar.ServletsAuxMethodsClass;
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
	 * @exception LocalUserNameFixesClassException
	 *                if owner is empty or null.
	 * @exception Exception
	 *                if it cannot be removed.
	 */
	public static void removeProgramFile(String fileOwner, String fileName, String localUserName) throws Exception {

		if (fileName == null) {
			throw new Exception("fileName is null");
		}
		ServletsAuxMethodsClass.checkUserNameIsValid(fileOwner);
		ServletsAuxMethodsClass.checkUserNameIsValid(localUserName);

		Boolean retVal = false;
		if (fileOwner.equals(localUserName)) {
			PathsMgmt pathsMgmt = new PathsMgmt();
			String fullPath = pathsMgmt.getFullPathOf(fileOwner, fileName, false);

			File file = new File(fullPath);
			retVal = file.exists();
			if (!retVal) {
				throw new Exception("The program file" + fullPath + "does not exist.");
			}
			retVal = file.delete();
			if (!retVal) {
				throw new Exception("The program file" + fullPath + "can not be removed.");
			}
		} else {
			throw new Exception("You do not own the program file.");
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
	 * @throws Exception
	 */
	public static Iterator<FileInfoClass> returnFilesIterator(String localUserName, Log LOG) throws Exception {

		LOG.info("localUserName: " + localUserName);
		ServletsAuxMethodsClass.checkUserNameIsValid(localUserName);

		Iterator<FileInfoClass> programFilesIterator = null;
		ArrayList<FileInfoClass> programFilesList = listProgramFiles(localUserName, LOG);
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
	 * @exception LocalUserNameFixesClassException
	 *                if owner is empty or null.
	 * @exception Exception
	 *                if there is some problem with a subfolder.
	 */
	private static ArrayList<FileInfoClass> listProgramFiles(String localUserName, Log LOG) throws Exception {

		LOG.info("localUserName: " + localUserName);

		PathsMgmt pathsMgmt = new PathsMgmt();
		File dir = new File(pathsMgmt.getProgramFilesPath());

		ArrayList<FileInfoClass> currentList = new ArrayList<FileInfoClass>();

		FilenameFilter filter;
		String[] subDirs;

		// We list first the localUserName program files.
		ServletsAuxMethodsClass.checkUserNameIsValid(localUserName);
		filter = (FilenameFilter) new OnlyLocalUserNameFolderFilterClass(localUserName);
		subDirs = dir.list(filter);

		if (subDirs != null) {
			for (int i = 0; i < subDirs.length; i++) {
				// Get filename of file or directory
				currentList = listProgramFilesInSubDir(subDirs[i], currentList);
			}
		}

		// We list in second (and last) place the other program files.
		ServletsAuxMethodsClass.checkUserNameIsValid(localUserName);
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
	 * @exception LocalUserNameFixesClassException
	 *                if owner is empty or null.
	 * @exception Exception
	 *                if there is some problem with a subfolder.
	 */
	private static ArrayList<FileInfoClass> listProgramFilesInSubDir(String subDir, ArrayList<FileInfoClass> currentList) throws Exception {

		if ((subDir == null) || ("".equals(subDir))) {
			throw new Exception("listProgramFilesInSubDir: subDir cannot be null nor empty string.");
		}

		PathsMgmt pathsMgmt = new PathsMgmt();
		String realPathSubDir = PathsMgmt.concatSubPaths(pathsMgmt.getProgramFilesPath(), subDir);

		File dir = new File(realPathSubDir);
		FilenameFilter filter = (FilenameFilter) new OnlyCiaoPrologFilesFilterClass();
		String[] files = dir.list(filter);

		if (files != null) {
			for (int i = 0; i < files.length; i++) {
				// Get filename of file or directory
				currentList.add(new FileInfoClass(files[i], subDir));
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
