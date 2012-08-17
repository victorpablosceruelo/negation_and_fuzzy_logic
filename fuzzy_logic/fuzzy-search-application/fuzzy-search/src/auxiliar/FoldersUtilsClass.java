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
		configureProgramsPath();
		plServerPath = lookForPlServer();
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
		configureProgramsPath();
		return programsPath;
	}
	
	/**
	 * Configures the attribute programsPath (if possible)  
	 * by assigning to it the value of a path where the programs can be 
	 * stored.
	 */
	private void configureProgramsPath() throws FoldersUtilsClassException {

		// Try the different options one by one.
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
	private Boolean testOrCreateProgramsPath(String newProgramsPath, Boolean createIfDoesNotExist) 
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
	
	public ArrayList<DataBaseInfoClass> listDatabases(String localUserName) throws FoldersUtilsClassException, LocalUserNameFixesClassException {
		
		configureProgramsPath();		
		File dir = new File(programsPath);
		ArrayList<DataBaseInfoClass> currentList = new ArrayList<DataBaseInfoClass>();

		FilenameFilter filter;
		String[] subDirs;
		
		LocalUserNameFixesClass.checkValidLocalUserName(localUserName);
		filter = (FilenameFilter) new OnlyLocalUserNameFolderFilterClass(localUserName);
		subDirs = dir.list(filter);

		if (subDirs != null) {
		    for (int i=0; i<subDirs.length; i++) {
		        // Get filename of file or directory
		    	currentList= listDatabasesInSubDir(subDirs[i], currentList);
		    }
		}

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

	private ArrayList<DataBaseInfoClass> listDatabasesInSubDir(String subDir, ArrayList<DataBaseInfoClass> currentList) throws FoldersUtilsClassException {

		configureProgramsPath();
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
	
	public String getPlServerPath() throws FoldersUtilsClassException {
		if (plServerPath == null) {
			plServerPath = lookForPlServer();
		}
		if (plServerPath == null) {
			throw new FoldersUtilsClassException("getPlServerPath: plServerPath is null.");
		}
		return plServerPath;
	}
	
	private String lookForPlServer() throws FoldersUtilsClassException {
		String pathOfPlServer = null;
		pathOfPlServer = lookForPlServerAux(pathOfPlServer, "/usr/lib/ciao/ciao-1.15/library/javall/plserver");
		pathOfPlServer = lookForPlServerAux(pathOfPlServer, "/home/vpablos/secured/CiaoDE_trunk/ciao/library/javall/plserver");
		pathOfPlServer = lookForPlServerAux(pathOfPlServer, "/home/vpablos/tmp/ciao-prolog-1.15.0+r14854/ciao/library/javall/plserver");
		pathOfPlServer = lookForPlServerAux(pathOfPlServer, "/home/tomcat/ciao-prolog-1.15.0+r14854/ciao/library/javall/plserver");
		
		// ToDo: Convendria un mecanismo algo más avanzado ... :-(
		lookForPlServerAdvanced(pathOfPlServer, "/usr/lib/ciao");
		lookForPlServerAdvanced(pathOfPlServer, "/usr/share/CiaoDE");
		lookForPlServerAdvanced(pathOfPlServer, "/home/");
		lookForPlServerAdvanced(pathOfPlServer, "/");
		
		if (pathOfPlServer == null) {
			throw new FoldersUtilsClassException("lookForPlServer: impossible to find plserver.");
		}
		
		return pathOfPlServer;
	}
	
	private String lookForPlServerAdvanced(String pathOfPlServer, String folderPath) {
		if (pathOfPlServer == null) {
			File currentDir = new File(folderPath);
			File[] subFiles = currentDir.listFiles();
			File file = null;
			int counter;
			
			// Test first the files.
			counter = 0;
			while ((pathOfPlServer == null) && (counter<subFiles.length)) {
				file = subFiles[counter];
				if (file.isFile()) {
					if ("plserver".equals(file.getName())) {
						pathOfPlServer = lookForPlServerAux(pathOfPlServer, file.getAbsolutePath());
					}
				}
				counter++;
			}

			// And at last the directories.
			counter = 0;
			while ((pathOfPlServer == null) && (counter<subFiles.length)) {
				file = subFiles[counter];
				if (file.isDirectory() && file.canRead() && file.canExecute()) {
					pathOfPlServer = lookForPlServerAdvanced(pathOfPlServer, file.getAbsolutePath());
				}
				counter++;
			}
		}
		return pathOfPlServer;
	}
	
	private String lookForPlServerAux(String pathOfPlServer, String executableFileWithPath) {
		if (pathOfPlServer == null) {
			File file = new File(executableFileWithPath);
			if (file.exists() && file.isFile() && file.canRead() && file.canExecute()) {
				pathOfPlServer = executableFileWithPath;
			}
		}
		return pathOfPlServer;
	}	
	
}
