package auxiliar;


import java.io.File;
import java.io.FilenameFilter;
import java.util.ArrayList;
import java.util.Iterator;

import org.apache.commons.logging.Log;

public class FilesMgmtClass {
	
	/**
	 * Obtains the complete path of the owner. 
	 * 
	 * @param    fileOwner It is the user for which we compute the path.
	 * @param 	 createIfDoesNotExist If yes and the whole path does not exists the method creates it.
	 * @return   the complete path where the owner can read/store his/her program files.
	 * @exception LocalUserNameFixesClassException if the owner string is empty or null
	 * @exception Exception if the folder can not be created
	 */
	public String getCompletePathFromFileOwner(String fileOwner, Boolean createIfDoesNotExist) throws Exception {

		ServletsAuxMethodsClass.checkUserNameIsValid(fileOwner);
		String userProgramFilesPath = programFilesPath + fileOwner + "/";
		testOrCreateProgramFilesPath(userProgramFilesPath, createIfDoesNotExist);
		return userProgramFilesPath ;
	}

	/**
	 * Obtains the complete path of the program file. 
	 * 
	 * @param    fileOwner It is the owner of the program file.
	 * @param    fileName It is the name of the file for which we are computing the path.
	 * @return   the complete path where the owner can read/store his/her files.
	 * @exception LocalUserNameFixesClassException if the owner string is empty or null
	 * @exception Exception if the folder or the program file do not exist or are invalid.
	 */
	public String getCompletePathOfProgramFile(String fileOwner, String fileName) throws Exception {

		ServletsAuxMethodsClass.checkUserNameIsValid(fileOwner);
		String ownerPath = programFilesPath + fileOwner + "/";
		testOrCreateProgramFilesPath(ownerPath, false);
		String programFilePath = ownerPath + fileName;
		File file = new File(programFilePath);
		if ((! file.exists()) || (! file.isFile()) || (! file.canRead())) {
			throw new Exception("getCompletePathOfProgramFile: file does not exist or is invalid.");
		}
		return programFilePath;
	}

	/**
	 * If the programFilesPath attribute is null or an empty string 
	 * checks if the path proposed in newProgramFilesPath
	 * serves for this purpose, and if so it modifies programFilesPath
	 * with its value.
	 * 
	 * @param newProgramFilesPath is the new path to test.
	 */
	static public String returnProgramFilesValidPath(String [] programFilesValidPaths) throws Exception {
		String programFilesValidPath = null;
		int index = 0;
				
		while (((programFilesValidPath == null) || ("".equals(programFilesValidPath))) && 
				(index < programFilesValidPaths.length)) {
			programFilesValidPath = programFilesValidPaths[index];
			try {
				if (! testOrCreateProgramFilesPath(programFilesValidPath, true)) {
					programFilesValidPath = null;
				}
			}
			catch (Exception e) {
				programFilesValidPath = null;
			}
		}
		
		if ((programFilesValidPath == null) || ("".equals(programFilesValidPath))) 
			throw new Exception("programFilesValidPath cannot be null.");
		return programFilesValidPath;
	}
	
	/**
	 * Tests whether the folder exists or not. If not and createIfDoesNotExist is true 
	 * the it creates it.
	 * 
	 * @param     newProgramFilesPath is the path we are checking.
	 * @param 	  createIfDoesNotExist If yes and the whole path does not exists the method creates it.
	 * @return    true if the folder was there or has been created. False otherwise. 
	 * @exception Exception if the folder can not be created
	 */
	public static Boolean testOrCreateProgramFilesPath(String newProgramFilesPath, Boolean createIfDoesNotExist) 
			throws Exception {
		
		msgs += "testOrCreateuserProgramFilesPath: newProgramFilesPath: " + newProgramFilesPath + " createIfDoesNotExist: " +
				createIfDoesNotExist;
		boolean retval = false;
		
		if ((newProgramFilesPath==null) || (newProgramFilesPath.equals(""))){
			throw new Exception("testOrCreateuserProgramFilesPath: newProgramFilesPath cannot be null nor empty string.");
		}
		else {
			
			File dir = new File(newProgramFilesPath); 
			if (dir.exists()) {
				if (dir.isDirectory() && dir.canRead() && dir.canWrite() && dir.canExecute()) {
					retval = true;	
				}
			}
			else {
				if (createIfDoesNotExist) {			
					try {
						retval = dir.mkdirs();
					} 
					catch (Exception ex) {
						msgs += "configureProgramFilesPathAux: not valid: " + newProgramFilesPath;
						if ((ex != null) && (ex.getMessage() != null)) 
							msgs += "Exception: " + ex.getMessage();
					}
				}
			}
		}
		return retval;
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
	public void removeProgramFile(String fileName, String fileOwner, String localUserName) throws Exception {

		msgs += "programFileName: "+fileName+" owner: "+fileOwner+" localUserName: "+localUserName;
		
		if (fileName == null) { throw new Exception("fileName is null"); }
		ServletsAuxMethodsClass.checkUserNameIsValid(fileOwner);
		ServletsAuxMethodsClass.checkUserNameIsValid(localUserName);		
		
		Boolean retVal = false;
		if (fileOwner.equals(localUserName)) {
			String ownerProgramFilesPath = getCompletePathFromFileOwner(fileOwner, false);
			String fileToRemove=ownerProgramFilesPath+fileName;
			
			File file = new File(fileToRemove);
			retVal = file.exists();
			if (! retVal) {
				throw new Exception("The program file" + fileToRemove + "does not exist.");
			}
			retVal = file.delete();
			if (! retVal) {
				throw new Exception("The program file" + fileToRemove + "can not be removed.");
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
	public Iterator<FileInfoClass> returnFilesIterator(String localUserName) throws Exception {
		Iterator<FileInfoClass> programFilesIterator = null;
		ArrayList<FileInfoClass> programFilesList = listProgramFiles(localUserName);
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
	private ArrayList<FileInfoClass> listProgramFiles(String localUserName) throws Exception {
				
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
		    	currentList= listProgramFilesInSubDir(subDirs[i], currentList);
		    }
		}

		// We list in second (and last) place the other program files.
		ServletsAuxMethodsClass.checkUserNameIsValid(localUserName);
		filter = (FilenameFilter) new OnlyNotLocalUserNameFolderFilterClass(localUserName);
		subDirs = dir.list(filter);

		if (subDirs != null) {
		    for (int i=0; i<subDirs.length; i++) {
		        // Get filename of file or directory
		    	currentList= listProgramFilesInSubDir(subDirs[i], currentList);
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
	private ArrayList<FileInfoClass> listProgramFilesInSubDir(String subDir, ArrayList<FileInfoClass> currentList) 
			throws Exception {

		msgs += "listProgramFilesInSubDir: subDir: " + subDir;
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
	
	/**
	 * Looks which one of the proposed subPaths for the plServer is the correct one.
	 * 
	 * @param plServerValidSubPaths are the new proposed subpaths for the plServer.
	 * @throws Exception when none is valid.
	 * 
	 */
	public String returnPlServerValidPath(String [] plServerValidSubPaths, Log LOG) throws Exception {
		String plServerPath = null;
		String plServerSubPath = null;
		int index = 0;
		
		while (((plServerPath == null) || ("".equals(plServerPath))) &&
				(index < plServerValidSubPaths.length)) {
			plServerSubPath = plServerValidSubPaths[index];

			plServerPath = lookForFileInSubDir("plserver", plServerSubPath, LOG);
		}
		
		if (plServerPath == null) {
			throw new Exception("plServerPath is null.");
		}
		return plServerPath;
	}
	
	private String lookForFileInSubDir(String fileName, String subPath, Log LOG) {
		String result = null;
		
		if (subPath != null) {
			File file_1 = new File(subPath);
			
			if ((file_1.exists()) && (file_1.canRead()) || (file_1.canExecute())) {
				if (file_1.isFile()) {
					if (fileName.equals(file_1.getName())) {
						result = subPath;
					}
				}
				else {
					if (file_1.isDirectory()) {
						File[] subFiles = file_1.listFiles();
						int index = 0;
						while ((result == null) && (index < subFiles.length)) {
							result = lookForFileInSubDir(fileName, subFiles[index].getAbsolutePath(), LOG);
						}
					}
					else {
						LOG.info("Impossible to process path (not a file nor a directory): " + subPath);
					}
				}
			}
			else {
				LOG.info("Impossible to process path (not exists, not readable or not executable): " + subPath);
			}
		}
		return result;
	}
	
	/**
	 * If the path of the plServer is not configured yet, 
	 * it looks for the plServer executable in the path given.
	 * If it is a valid path, it just sets the attribute plServerPath.
	 * 
	 * @param untestedPathForPlServer is the new proposed path for the plServer.
	 * @throws Exception when untestedPathForPlServer is empty string or null.
	 * 
	 */
	private void configurePlServerPathAux(String untestedPathForPlServer) throws Exception {
		if (plServerPath == null) {
			if ((untestedPathForPlServer == null) || ("".equals(untestedPathForPlServer))) {
				throw new Exception("configurePlServerPathAux: untestedPathForPlServer is empty string or null.");
			}
			File file = new File(untestedPathForPlServer);
			if ((file.exists()) && (file.isFile()) && (file.canRead()) && (file.canExecute()) && (file.getName().equals("plserver"))) {
				plServerPath = untestedPathForPlServer;
			}
		}
	}	
	
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////

	/**
	 * Checks if the folder with path folderPath exists, is a directory, can be read
	 * and the files inside can be accessed. 
	 * 
	 * @param folderPath is the path of the folder.
	 * @param relativePath if true then programFilesPath + relativePath will be used instead.
	 * @throws Exception when folderPath is empty string or null.
	 * 
	 */
	public static Boolean folderExists (String folderPath, Boolean relativePath) throws Exception {
		
		msgs += "folderExists: folderPath: " + folderPath;
		if ((folderPath == null) || ("".equals(folderPath))) {
			throw new Exception("folderExists: folderPath is empty string or null.");
		}
		if (relativePath) {
			folderPath = programFilesPath + folderPath;
		}
		File dir = new File(folderPath);
		Boolean retVal = ((dir.exists()) && (dir.isDirectory()) && (dir.canRead()) && (dir.canExecute()));
		msgs += "dir.exists(): " + dir.exists() + " dir.isDirectory(): " + dir.isDirectory();
		msgs += "dir.canRead(): " + dir.canRead() + " dir.canExecute(): " + dir.canExecute();
		return retVal;
	}
	
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public Boolean programFileExists(String fileOwner, String fileName, Boolean relativePath) throws Exception {
		
		msgs += "programFileExists: fileOwner: " + fileOwner + " fileName: " + fileName;
		if ((fileOwner == null) || ("".equals(fileOwner))) {
			throw new Exception("programFileExists: fileOwner is empty string or null.");
		}
		if ((fileName == null) || ("".equals(fileName))) {
			throw new Exception("programFileExists: fileName is empty string or null.");
		}
		
		String fullPath = null;
		if (relativePath) {
			fullPath = programFilesPath + fileOwner + "/" + fileName;
		}
		File file = new File(fullPath);
		Boolean retVal = ((file.exists()) && (file.isFile()) && (file.canRead()));
		msgs += "file.exists(): " + file.exists() + " file.isFile(): " + file.isFile();
		msgs += "file.canRead(): " + file.canRead();
		return retVal;
	}

	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public String getMsgs () {
		String retValue = msgs;
		msgs = ""; // Re-initialize
		return retValue;
	}
	
}
