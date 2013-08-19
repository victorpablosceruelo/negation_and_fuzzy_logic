package filesAndPaths;


import java.io.File;
import java.io.FilenameFilter;
import java.util.ArrayList;
import java.util.Iterator;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import constants.KConstants;

import auxiliar.DispatchersClass;
import auxiliar.FileInfoClass;
import auxiliar.ServletsAuxMethodsClass;

import filters.OnlyCiaoPrologFilesFilterClass;
import filters.OnlyLocalUserNameFolderFilterClass;
import filters.OnlyNotLocalUserNameFolderFilterClass;

public class FilesMgmt {
	

	
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////

	
	/**
	 * Obtains the complete path of the program file. 
	 * 
	 * @param	 programFilesPath is the path where all the program files are stored.
	 * @param    fileOwner It is the owner of the program file.
	 * @param    fileName It is the name of the file for which we are computing the path.
	 * @param    createFolderIfDoesNotExist allows to create the folder if it does not exist.
	 * @return   the complete path for the fileName (if it is not null) or for the fileOwner.
	 * @exception LocalUserNameFixesClassException if the owner string is empty or null
	 * @exception Exception if programFilesPath is null, if fileOwner is null or if the program file does not exist or is invalid.
	 */
	public static String getFullPath(String programFilesPath, String fileOwner, String fileName, Boolean createFolderIfDoesNotExist) throws Exception {

		if ((programFilesPath == null) || ("".equals(programFilesPath))) {
			throw new Exception("programFilesPath is empty string or null.");
		}
		if (! ((fileOwner == null) && (fileName == null))) {
			ServletsAuxMethodsClass.checkUserNameIsValid(fileOwner);
		}
		
		String fullPath = null;

		if ((programFilesPath != null) && (! "".equals(programFilesPath)) && (fileOwner != null)) {
			if (programFilesPath.endsWith("/")) {
				fullPath = programFilesPath + fileOwner;
			}
			else {
				fullPath = programFilesPath + "/" + fileOwner;
			}
		}
		else {
			fullPath = programFilesPath;
		}


		File dir = new File(fullPath); 
		if (dir.exists()) {
			if (! (dir.isDirectory() && dir.canRead() && dir.canWrite() && dir.canExecute())) {
				fullPath = null;
			}
		}
		else {
			if (createFolderIfDoesNotExist) {			
				try {
					if (! dir.mkdirs()) {
						fullPath = null;
					}
				} 
				catch (Exception ex) {
					fullPath = null;
					throw new Exception("The folder " + fileOwner + "can not be created.");
				}
			}
		}
				
		if ((fullPath != null) && (fileName != null) && (! "".equals(fileName))) {
			if (fullPath.endsWith("/")) fullPath += fileName;
			else fullPath += "/" + fileName;
				
			File file = new File(fullPath);
			if (! file.exists()) {
				throw new Exception("file does not exist. file: " + fullPath);
			}
			if (! file.isFile()) {
				throw new Exception("file is not a file. file: " + fullPath);
			}
			if (! file.canRead()) {
				throw new Exception("file is not readable. file: " + fullPath);
			}
		}
		return fullPath;
	}
	
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public Boolean programFileExists(String programFilesPath, String fileOwner, String fileName) throws Exception {
		
		if ((fileOwner == null) || ("".equals(fileOwner))) {
			throw new Exception("fileOwner is empty string or null.");
		}
		if ((fileName == null) || ("".equals(fileName))) {
			throw new Exception("fileName is empty string or null.");
		}
		if ((programFilesPath == null) || ("".equals(programFilesPath))) {
			throw new Exception("programFilesPath is empty string or null.");
		}

		String fullPath = programFilesPath + "/" + fileOwner + "/" + fileName;
		File file = new File(fullPath);
		return ((file.exists()) && (file.isFile()) && (file.canRead()));
	}
	
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////

	/**
	 * Removes the program file, but only if its fileOwner is the localUserName.
	 * 
	 * @param     fileName is the name of the file to remove.
	 * @param     fileOwner is the owner of the file to be removed, and its relative path.
	 * @param     localUserName is the name of the user that requests its removal.
	 * @exception LocalUserNameFixesClassException if owner is empty or null.
	 * @exception Exception if it cannot be removed.
	 */
	public static void removeProgramFile(String programFilesPath, String fileOwner, String fileName, String localUserName) throws Exception {
		
		if (fileName == null) { throw new Exception("fileName is null"); }
		ServletsAuxMethodsClass.checkUserNameIsValid(fileOwner);
		ServletsAuxMethodsClass.checkUserNameIsValid(localUserName);		
		
		Boolean retVal = false;
		if (fileOwner.equals(localUserName)) {
			String fullPath = getFullPath(programFilesPath, fileOwner, fileName, false);
			
			File file = new File(fullPath);
			retVal = file.exists();
			if (! retVal) {
				throw new Exception("The program file" + fullPath + "does not exist.");
			}
			retVal = file.delete();
			if (! retVal) {
				throw new Exception("The program file" + fullPath + "can not be removed.");
			}
		}
		else {
			throw new Exception("You do not own the program file.");
		}
	}
	
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	/**
	 * Gets an iterator to iterate on the existing program files.
	 * 
	 * @param     localUserName is the name of the user that is logged in.
	 * @return    the program files iterator, null if there are no program files to iterate.
	 * @throws Exception 
	 */
	public static Iterator<FileInfoClass> returnFilesIterator(String programFilesPath, String localUserName, Log LOG) throws Exception {
		
		LOG.info("programFilesPath: " + programFilesPath + " localUserName: " + localUserName);
		ServletsAuxMethodsClass.checkUserNameIsValid(localUserName);
		
		Iterator<FileInfoClass> programFilesIterator = null;
		ArrayList<FileInfoClass> programFilesList = listProgramFiles(programFilesPath, localUserName, LOG);
		if ((programFilesList != null) && (! programFilesList.isEmpty())) {
			programFilesIterator = programFilesList.iterator(); 
		}
		return programFilesIterator;
	}
	
	/**
	 * Gets a list with the existing program files.
	 * 
	 * @param     localUserName is the name of the user that is logged in.
	 * @return    the program files iterator, null if there are no program files to iterate.
	 * @exception LocalUserNameFixesClassException if owner is empty or null.
	 * @exception Exception if there is some problem with a subfolder.
	 */
	private static ArrayList<FileInfoClass> listProgramFiles(String programFilesPath, String localUserName, Log LOG) throws Exception {
		
		LOG.info("programFilesPath: " + programFilesPath + " localUserName: " + localUserName);
		
		File dir = new File(programFilesPath);
		ArrayList<FileInfoClass> currentList = new ArrayList<FileInfoClass>();

		FilenameFilter filter;
		String[] subDirs;
		
		// We list first the localUserName program files.
		ServletsAuxMethodsClass.checkUserNameIsValid(localUserName);
		filter = (FilenameFilter) new OnlyLocalUserNameFolderFilterClass(localUserName);
		subDirs = dir.list(filter);

		if (subDirs != null) {
		    for (int i=0; i<subDirs.length; i++) {
		        // Get filename of file or directory
		    	currentList= listProgramFilesInSubDir(programFilesPath, subDirs[i], currentList);
		    }
		}

		// We list in second (and last) place the other program files.
		ServletsAuxMethodsClass.checkUserNameIsValid(localUserName);
		filter = (FilenameFilter) new OnlyNotLocalUserNameFolderFilterClass(localUserName);
		subDirs = dir.list(filter);

		if (subDirs != null) {
		    for (int i=0; i<subDirs.length; i++) {
		        // Get filename of file or directory
		    	currentList= listProgramFilesInSubDir(programFilesPath, subDirs[i], currentList);
		    }
		}
		
		return currentList;
	}

	/**
	 * Gets a list with the existing program files.
	 * 
	 * @param     subDir is the full path of the subdirectory we are listing.
	 * @return    the program files iterator, null if there are no program files to iterate.
	 * @exception LocalUserNameFixesClassException if owner is empty or null.
	 * @exception Exception if there is some problem with a subfolder.
	 */
	private static ArrayList<FileInfoClass> listProgramFilesInSubDir(String programFilesPath, String subDir, ArrayList<FileInfoClass> currentList) 
			throws Exception {

		if ((subDir == null) || ("".equals(subDir))) {
			throw new Exception("listProgramFilesInSubDir: subDir cannot be null nor empty string.");
		}
		String realPathSubDir = programFilesPath + subDir + "/";
		File dir = new File(realPathSubDir);
		FilenameFilter filter = (FilenameFilter) new OnlyCiaoPrologFilesFilterClass();
		String[] files = dir.list(filter);
		
		if (files != null) {
		    for (int i=0; i<files.length; i++) {
		        // Get filename of file or directory
		    	currentList.add(new FileInfoClass(files[i], subDir));
		    }
		}
		return currentList;
	}
	
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////

}
