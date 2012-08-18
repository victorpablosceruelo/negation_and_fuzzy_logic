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
	private static String programsPath = null;
	private static String plServerPath = null;
	
	public FoldersUtilsClass() throws FoldersUtilsClassException {
		
		// Configure the programsPath: Try the different options one by one.
		configureProgramsPathAux("/home/java-apps/fuzzy-search/");
		configureProgramsPathAux(System.getProperty("java.io.tmpdir") + "/java-apps/fuzzy-search/"); 
		// configureProgramsPathAux(servlet.getServletContext().getInitParameter("working-folder-fuzzy-search"));
		configureProgramsPathAux("/tmp/java-apps/fuzzy-search/");

		if ((programsPath == null) || (programsPath.equals(""))) {
			throw new FoldersUtilsClassException("configureProgramsPath: Cannot configure the path for the programs.");
		}
		else {
			LOG.info("choosen folder for uploads: " + programsPath);
		}
		
		// Configure plServer path
		configurePlServerPathAux("/usr/lib/ciao/ciao-1.15/library/javall/plserver");
		configurePlServerPathAux("/home/vpablos/secured/CiaoDE_trunk/ciao/library/javall/plserver");
		configurePlServerPathAux("/home/vpablos/tmp/ciao-prolog-1.15.0+r14854/ciao/library/javall/plserver");
		configurePlServerPathAux("/home/tomcat/ciao-prolog-1.15.0+r14854/ciao/library/javall/plserver");
		
		// ToDo: Convendria un mecanismo algo más avanzado ... :-(
		configurePlServerPathAdvanced("/usr/lib/ciao");
		configurePlServerPathAdvanced("/usr/share/CiaoDE");
		configurePlServerPathAdvanced("/home/");
		configurePlServerPathAdvanced("/");
		
		if (plServerPath == null) {
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
	 * @return   the complete path where the owner can read/store his/her databases.
	 * @exception LocalUserNameFixesClassException if the owner string is empty or null
	 * @exception FoldersUtilsClassException if the folder can not be created
	 */
	public String getCompletePathOfOwner(String owner, Boolean createIfDoesNotExist) 
			throws FoldersUtilsClassException, LocalUserNameFixesClassException {

		LocalUserNameFixesClass.checkValidLocalUserName(owner);
		String userProgramsPath = programsPath + owner + "/";
		testOrCreateProgramsPath(userProgramsPath, createIfDoesNotExist);
		return userProgramsPath ;
	}
	
	/**
	 * Obtains the complete path of the folder in which programs can be found
	 * and/or stored.
	 *  
	 * @return the path in which programs can be found and/or stored.
	 */
	public String getprogramsPath() throws FoldersUtilsClassException {
		return programsPath;
	}

	/**
	 * If the programsPath attribute is null or an empty string 
	 * checks if the path proposed in newProgramsPath
	 * serves for this purpose, and if so it modifies programsPath
	 * with its value.
	 * 
	 * @param newProgramsPath is the new path to test.
	 */
	private void configureProgramsPathAux(String newProgramsPath) throws FoldersUtilsClassException {
		LOG.info("configureProgramsPathAux: testing: " + newProgramsPath);
		
		if ((programsPath == null) || (programsPath.equals(""))) {
			if (testOrCreateProgramsPath(newProgramsPath, true)) {
				programsPath = newProgramsPath;
			}
		}
	}
	
	/**
	 * Tests whether the folder exists or not. If not and createIfDoesNotExist is true 
	 * the it creates it.
	 * 
	 * @param     newProgramsPath is the path we are checking.
	 * @param 	  createIfDoesNotExist If yes and the whole path does not exists the method creates it.
	 * @return    true if the folder was there or has been created. False otherwise. 
	 * @exception FoldersUtilsClassException if the folder can not be created
	 */
	public Boolean testOrCreateProgramsPath(String newProgramsPath, Boolean createIfDoesNotExist) 
			throws FoldersUtilsClassException {
		boolean retval = false;
		
		if ((newProgramsPath==null) || (newProgramsPath.equals(""))){
			throw new FoldersUtilsClassException("testOrCreateuserProgramsPath: newProgramsPath cannot be null nor empty string.");
		}
		else {
			
			File dir = new File(newProgramsPath); 
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
						LOG.info("configureProgramsPathAux: not valid: " + newProgramsPath);
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
	 * @return    true if the database was removed. False otherwise.
	 * @exception LocalUserNameFixesClassException if owner is empty or null.
	 * @exception FoldersUtilsClassException never.
	 */
	public Boolean removeDataBase(String database, String owner, String localUserName) 
			throws FoldersUtilsClassException, LocalUserNameFixesClassException {

		Boolean retval = false;
		if (owner == localUserName) {
			String ownerProgramsPath = getCompletePathOfOwner(owner, false);
			String fileToRemove=ownerProgramsPath+database;
			
			try {
				File file = new File(fileToRemove);
				if (file.exists()) {
					retval = file.delete();
				}
			} 
			catch (Exception ex) {
				LOG.info("configureProgramsPathAux: not valid: " + ownerProgramsPath);
				LOG.info("Exception: " + ex);
			}
		}

		return retval;
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
	public Iterator<DataBaseInfoClass> returnDatabasesIterator(String localUserName) {
		Iterator<DataBaseInfoClass> databasesIterator = null;
		try {
			ArrayList<DataBaseInfoClass> databasesList = listDatabases(localUserName);
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
	private ArrayList<DataBaseInfoClass> listDatabases(String localUserName) 
			throws FoldersUtilsClassException, LocalUserNameFixesClassException {
				
		File dir = new File(programsPath);
		ArrayList<DataBaseInfoClass> currentList = new ArrayList<DataBaseInfoClass>();

		FilenameFilter filter;
		String[] subDirs;
		
		// We list first the localUserName databases.
		LocalUserNameFixesClass.checkValidLocalUserName(localUserName);
		filter = (FilenameFilter) new OnlyLocalUserNameFolderFilterClass(localUserName);
		subDirs = dir.list(filter);

		if (subDirs != null) {
		    for (int i=0; i<subDirs.length; i++) {
		        // Get filename of file or directory
		    	currentList= listDatabasesInSubDir(subDirs[i], currentList);
		    }
		}

		// We list in second (and last) place the other databases.
		LocalUserNameFixesClass.checkValidLocalUserName(localUserName);
		filter = (FilenameFilter) new OnlyNotLocalUserNameFolderFilterClass(localUserName);
		subDirs = dir.list(filter);

		if (subDirs != null) {
		    for (int i=0; i<subDirs.length; i++) {
		        // Get filename of file or directory
		    	currentList= listDatabasesInSubDir(subDirs[i], currentList);
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
	private ArrayList<DataBaseInfoClass> listDatabasesInSubDir(String subDir, ArrayList<DataBaseInfoClass> currentList) 
			throws FoldersUtilsClassException {

		if ((subDir == null) || ("".equals(subDir))) {
			throw new FoldersUtilsClassException("listDatabasesInSubDir: subDir cannot be null nor empty string.");
		}
		String realPathSubDir = programsPath + subDir + "/";
		File dir = new File(realPathSubDir);
		FilenameFilter filter = (FilenameFilter) new OnlyCiaoPrologFilesFilterClass();
		String[] files = dir.list(filter);
		
		if (files != null) {
		    for (int i=0; i<files.length; i++) {
		        // Get filename of file or directory
		    	currentList.add(new DataBaseInfoClass(files[i], subDir));
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
	 * 
	 */
	private void configurePlServerPathAdvanced(String subPath) {
		if (plServerPath == null) {
			File currentDir = new File(subPath);
			File[] subFiles = currentDir.listFiles();
			File file = null;
			int counter;
			
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
				if (file.isDirectory() && file.canRead() && file.canExecute()) {
					configurePlServerPathAdvanced(file.getAbsolutePath());
				}
				counter++;
			}
		}
	}
	
	/**
	 * If the path of the plServer is not configured yet, 
	 * it looks for the plServer executable in the path given.
	 * If it is a valid path, it just sets the attribute plServerPath.
	 * 
	 * @param untestedPathForPlServer is the new proposed path for the plServer.
	 * 
	 */
	private void configurePlServerPathAux(String untestedPathForPlServer) {
		if (plServerPath == null) {
			File file = new File(untestedPathForPlServer);
			if (file.exists() && file.isFile() && file.canRead() && file.canExecute()) {
				plServerPath = untestedPathForPlServer;
			}
		}
	}	
	
}
