package auxiliar;


import java.io.File;
import java.io.FilenameFilter;
import java.util.ArrayList;
import java.util.Iterator;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
// import javax.servlet.http.HttpServlet;
// import javax.servlet.annotation.WebServlet;
// import servlets.UploadFileServlet;
//import org.apache.commons.io.filefilter.WildcardFileFilter;

public class FoldersUtilsClass {

	// private static final long serialVersionUID = 1L;
	private static final Log LOG = LogFactory.getLog(FoldersUtilsClass.class);
	private static String programFilesPath = null;
	private static String plServerPath = null;
	
	public FoldersUtilsClass() throws FoldersUtilsClassException {
		
		if ((programFilesPath == null) || (programFilesPath.equals(""))) {
			LOG.info("looking for a folder for uploading program files ... ");
			// Configure the programFilesPath: Try the different options one by one.
			configureProgramFilesPathAux("/home/java-apps/fuzzy-search/");
			configureProgramFilesPathAux(System.getProperty("java.io.tmpdir") + "/java-apps/fuzzy-search/"); 
			// configureProgramFilesPathAux(servlet.getServletContext().getInitParameter("working-folder-fuzzy-search"));
			configureProgramFilesPathAux("/tmp/java-apps/fuzzy-search/");
		}
		
		if ((programFilesPath == null) || ("".equals(programFilesPath))) {
			throw new FoldersUtilsClassException("configureProgramFilesPath: Cannot configure the path for the programs.");
		}
		else {
			LOG.info("choosen folder for uploads: " + programFilesPath);
		}
		
		if ((plServerPath == null) || ("".equals(plServerPath))) {
			LOG.info("looking for the path of plserver ... ");
			// Configure plServer path
			// configurePlServerPathAux("/usr/lib/ciao/ciao-1.15/library/javall/plserver");
			// configurePlServerPathAux("/home/vpablos/secured/CiaoDE_trunk/ciao/library/javall/plserver");
			// configurePlServerPathAux("/home/vpablos/tmp/ciao-prolog-1.15.0+r14854/ciao/library/javall/plserver");
			configurePlServerPathAux("/home/tomcat/ciao-prolog-1.15.0+r14854/ciao/library/javall/plserver");

			// ToDo: Convendria un mecanismo algo más avanzado ... :-(
			configurePlServerPathAdvanced("/usr/lib/ciao");
			configurePlServerPathAdvanced("/usr/share/CiaoDE");
			configurePlServerPathAdvanced("/usr");
			configurePlServerPathAdvanced("/opt");
			configurePlServerPathAdvanced("/home");
			configurePlServerPathAdvanced("/");
		}
		
		if ((plServerPath == null) || ("".equals(plServerPath))) {
			throw new FoldersUtilsClassException("lookForPlServer: impossible to find plserver.");
		}
		else {
			LOG.info("plServer path: " + plServerPath);
		}
	}
	
	/**
	 * Obtains the complete path of the owner. 
	 * 
	 * @param    owner It is the user for which we compute the path.
	 * @param 	 createIfDoesNotExist If yes and the whole path does not exists the method creates it.
	 * @return   the complete path where the owner can read/store his/her program files.
	 * @exception LocalUserNameFixesClassException if the owner string is empty or null
	 * @exception FoldersUtilsClassException if the folder can not be created
	 */
	public String getCompletePathOfOwner(String owner, Boolean createIfDoesNotExist) 
			throws FoldersUtilsClassException, LocalUserNameFixesClassException {

		LocalUserNameFixesClass.checkValidLocalUserName(owner);
		String userProgramFilesPath = programFilesPath + owner + "/";
		testOrCreateProgramFilesPath(userProgramFilesPath, createIfDoesNotExist);
		LOG.info("getCompletePathOfOwner: owner: "+owner+" userProgramFilesPath: "+userProgramFilesPath);
		return userProgramFilesPath ;
	}

	/**
	 * Obtains the complete path of the program file. 
	 * 
	 * @param    owner It is the owner of the database.
	 * @param    database It is the database for which we are computing the path.
	 * @return   the complete path where the owner can read/store his/her databases.
	 * @exception LocalUserNameFixesClassException if the owner string is empty or null
	 * @exception FoldersUtilsClassException if the folder or the database do not exist or are invalid.
	 */
	public String getCompletePathOfProgramFile(String fileOwner, String fileName) 
			throws FoldersUtilsClassException, LocalUserNameFixesClassException {

		LocalUserNameFixesClass.checkValidLocalUserName(fileOwner);
		String ownerPath = programFilesPath + fileOwner + "/";
		testOrCreateProgramFilesPath(ownerPath, false);
		String databasePath = ownerPath + fileName;
		File file = new File(databasePath);
		if ((! file.exists()) || (! file.isFile()) || (! file.canRead())) {
			throw new FoldersUtilsClassException("getCompletePathOfDatabase: database does not exist or is invalid.");
		}
		return databasePath;
	}

	
	/**
	 * Obtains the complete path of the folder in which programs can be found
	 * and/or stored.
	 *  
	 * @return the path in which programs can be found and/or stored.
	 */
	public String getProgramFilesPath() throws FoldersUtilsClassException {
		return programFilesPath;
	}

	/**
	 * If the programFilesPath attribute is null or an empty string 
	 * checks if the path proposed in newProgramFilesPath
	 * serves for this purpose, and if so it modifies programFilesPath
	 * with its value.
	 * 
	 * @param newProgramFilesPath is the new path to test.
	 */
	private void configureProgramFilesPathAux(String newProgramFilesPath) throws FoldersUtilsClassException {
		LOG.info("configureProgramFilesPathAux: testing: " + newProgramFilesPath);
		
		if ((programFilesPath == null) || (programFilesPath.equals(""))) {
			if (testOrCreateProgramFilesPath(newProgramFilesPath, true)) {
				programFilesPath = newProgramFilesPath;
			}
		}
	}
	
	/**
	 * Tests whether the folder exists or not. If not and createIfDoesNotExist is true 
	 * the it creates it.
	 * 
	 * @param     newProgramFilesPath is the path we are checking.
	 * @param 	  createIfDoesNotExist If yes and the whole path does not exists the method creates it.
	 * @return    true if the folder was there or has been created. False otherwise. 
	 * @exception FoldersUtilsClassException if the folder can not be created
	 */
	public Boolean testOrCreateProgramFilesPath(String newProgramFilesPath, Boolean createIfDoesNotExist) 
			throws FoldersUtilsClassException {
		
		LOG.info("testOrCreateuserProgramFilesPath: newProgramFilesPath: " + newProgramFilesPath + " createIfDoesNotExist: " +
				createIfDoesNotExist);
		boolean retval = false;
		
		if ((newProgramFilesPath==null) || (newProgramFilesPath.equals(""))){
			throw new FoldersUtilsClassException("testOrCreateuserProgramFilesPath: newProgramFilesPath cannot be null nor empty string.");
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
						LOG.info("configureProgramFilesPathAux: not valid: " + newProgramFilesPath);
						LOG.info("Exception: " + ex);
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
	 * Removes the database only if its owner is the localUserName.
	 * 
	 * @param     database is the database to remove.
	 * @param     owner is the database owner and the relative path to the database.
	 * @param     localUserName is the name of the user that requests its removal.
	 * @exception LocalUserNameFixesClassException if owner is empty or null.
	 * @exception FoldersUtilsClassException if it cannot be removed.
	 */
	public void removeDataBase(String database, String owner, String localUserName) 
			throws FoldersUtilsClassException, LocalUserNameFixesClassException {

		LOG.info("database: "+database+" owner: "+owner+" localUserName: "+localUserName);
		
		if (database == null) { throw new FoldersUtilsClassException("database is null"); }
		if (owner == null) { throw new FoldersUtilsClassException("owner is null"); }
		if (localUserName == null) { throw new FoldersUtilsClassException("localUserName is null"); }
		
		
		Boolean retVal = false;
		if (owner.equals(localUserName)) {
			String ownerProgramFilesPath = getCompletePathOfOwner(owner, false);
			String fileToRemove=ownerProgramFilesPath+database;
			
			File file = new File(fileToRemove);
			retVal = file.exists();
			if (! retVal) {
				throw new FoldersUtilsClassException("The database file" + fileToRemove + "does not exist.");
			}
			retVal = file.delete();
			if (! retVal) {
				throw new FoldersUtilsClassException("The database file" + fileToRemove + "can not be removed.");
			}
		}
		else {
			throw new FoldersUtilsClassException("You do not own the database file.");
		}
	}
	
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	/**
	 * Gets an iterator to iterate on the existing databases.
	 * 
	 * @param     localUserName is the name of the user that is logged in.
	 * @return    the databases iterator, null if there are no databases to iterate.
	 */
	public Iterator<FileInfoClass> returnFilesIterator(String localUserName) {
		Iterator<FileInfoClass> databasesIterator = null;
		try {
			ArrayList<FileInfoClass> databasesList = listProgramFiles(localUserName);
			if (!databasesList.isEmpty()) {
				databasesIterator = databasesList.iterator(); 
			}
		} catch (Exception e) {
			LOG.info("Exception: " + e);
			e.printStackTrace();
			databasesIterator = null;
		}
		return databasesIterator;
	}
	
	/**
	 * Gets a list with the existing databases.
	 * 
	 * @param     localUserName is the name of the user that is logged in.
	 * @return    the databases iterator, null if there are no databases to iterate.
	 * @exception LocalUserNameFixesClassException if owner is empty or null.
	 * @exception FoldersUtilsClassException if there is some problem with a subfolder.
	 */
	private ArrayList<FileInfoClass> listProgramFiles(String localUserName) 
			throws FoldersUtilsClassException, LocalUserNameFixesClassException {
				
		File dir = new File(programFilesPath);
		ArrayList<FileInfoClass> currentList = new ArrayList<FileInfoClass>();

		FilenameFilter filter;
		String[] subDirs;
		
		// We list first the localUserName databases.
		LocalUserNameFixesClass.checkValidLocalUserName(localUserName);
		filter = (FilenameFilter) new OnlyLocalUserNameFolderFilterClass(localUserName);
		subDirs = dir.list(filter);

		if (subDirs != null) {
		    for (int i=0; i<subDirs.length; i++) {
		        // Get filename of file or directory
		    	currentList= listProgramFilesInSubDir(subDirs[i], currentList);
		    }
		}

		// We list in second (and last) place the other databases.
		LocalUserNameFixesClass.checkValidLocalUserName(localUserName);
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
	 * Gets a list with the existing databases.
	 * 
	 * @param     subDir is the full path of the subdirectory we are listing.
	 * @return    the databases iterator, null if there are no databases to iterate.
	 * @exception LocalUserNameFixesClassException if owner is empty or null.
	 * @exception FoldersUtilsClassException if there is some problem with a subfolder.
	 */
	private ArrayList<FileInfoClass> listProgramFilesInSubDir(String subDir, ArrayList<FileInfoClass> currentList) 
			throws FoldersUtilsClassException {

		LOG.info("listDatabasesInSubDir: subDir: " + subDir);
		if ((subDir == null) || ("".equals(subDir))) {
			throw new FoldersUtilsClassException("listDatabasesInSubDir: subDir cannot be null nor empty string.");
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
	 * Returns the value of the path of plserver
	 * 
	 * @return    the path of the executable file plServer.
	 * @exception FoldersUtilsClassException if plServer is null.
	 * 
	 */
	public String getPlServerPath() throws FoldersUtilsClassException {
		if (plServerPath == null) {
			throw new FoldersUtilsClassException("getPlServerPath: plServerPath is null.");
		}
		return plServerPath;
	}
	
	/**
	 * If the path of the plServer is not configured yet, 
	 * it looks for the plServer executable in the subpath given.
	 * 
	 * @param subPath is the new proposed subpath for the plServer.
	 * @throws FoldersUtilsClassException when subPath is an empty string or null,
	 * 
	 */
	private void configurePlServerPathAdvanced(String subPath) throws FoldersUtilsClassException {
		if (plServerPath == null) {
			if ((subPath == null) || ("".equals(subPath))) {
				throw new FoldersUtilsClassException("configurePlServerPathAdvanced: subPath is empty string or null.");
			}
			File currentDir = new File(subPath);
			if ((currentDir.exists()) || (currentDir.isDirectory()) || (currentDir.canRead()) || (currentDir.canExecute())) {
				File[] subFiles = currentDir.listFiles();
				File file = null;
				int counter;

				if (subFiles != null) {
					// Test first the files.
					counter = 0;
					while ((plServerPath == null) && (counter<subFiles.length)) {
						file = subFiles[counter];
						if (file.isFile()) {
							if ("plserver".equals(file.getName())) {
								configurePlServerPathAux(file.getAbsolutePath());
							}
						}
						counter++;
					}

					// And at last the directories.
					counter = 0;
					while ((plServerPath == null) && (counter<subFiles.length)) {
						file = subFiles[counter];
						if ((file.exists()) && (file.isDirectory()) && (file.canRead()) && (file.canExecute())) {
							configurePlServerPathAdvanced(file.getAbsolutePath());
						}
						counter++;
					}
				}
			}
		}
	}
	
	/**
	 * If the path of the plServer is not configured yet, 
	 * it looks for the plServer executable in the path given.
	 * If it is a valid path, it just sets the attribute plServerPath.
	 * 
	 * @param untestedPathForPlServer is the new proposed path for the plServer.
	 * @throws FoldersUtilsClassException when untestedPathForPlServer is empty string or null.
	 * 
	 */
	private void configurePlServerPathAux(String untestedPathForPlServer) throws FoldersUtilsClassException {
		if (plServerPath == null) {
			if ((untestedPathForPlServer == null) || ("".equals(untestedPathForPlServer))) {
				throw new FoldersUtilsClassException("configurePlServerPathAux: untestedPathForPlServer is empty string or null.");
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
	 * @throws FoldersUtilsClassException when folderPath is empty string or null.
	 * 
	 */
	public Boolean folderExists (String folderPath, Boolean relativePath) throws FoldersUtilsClassException {
		
		LOG.info("folderExists: folderPath: " + folderPath);
		if ((folderPath == null) || ("".equals(folderPath))) {
			throw new FoldersUtilsClassException("folderExists: folderPath is empty string or null.");
		}
		if (relativePath) {
			folderPath = programFilesPath + folderPath;
		}
		File dir = new File(folderPath);
		Boolean retVal = ((dir.exists()) && (dir.isDirectory()) && (dir.canRead()) && (dir.canExecute()));
		LOG.info("dir.exists(): " + dir.exists() + " dir.isDirectory(): " + dir.isDirectory());
		LOG.info("dir.canRead(): " + dir.canRead() + " dir.canExecute(): " + dir.canExecute());
		return retVal;
	}
	
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public Boolean programFileExists(String owner, String database, Boolean relativePath) throws FoldersUtilsClassException {
		
		LOG.info("databaseExists: owner: " + owner + " database: " + database);
		if ((owner == null) || ("".equals(owner))) {
			throw new FoldersUtilsClassException("databaseExists: owner is empty string or null.");
		}
		if ((database == null) || ("".equals(database))) {
			throw new FoldersUtilsClassException("databaseExists: database is empty string or null.");
		}
		
		String fullPath = null;
		if (relativePath) {
			fullPath = programFilesPath + owner + "/" + database;
		}
		File file = new File(fullPath);
		Boolean retVal = ((file.exists()) && (file.isFile()) && (file.canRead()));
		LOG.info("file.exists(): " + file.exists() + " file.isFile(): " + file.isFile());
		LOG.info("file.canRead(): " + file.canRead());
		return retVal;
	}
	
}
