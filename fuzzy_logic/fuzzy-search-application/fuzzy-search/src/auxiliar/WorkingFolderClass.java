package auxiliar;


import java.io.File;
import java.io.FilenameFilter;
import java.util.ArrayList;
import java.util.Iterator;

import org.apache.commons.io.filefilter.WildcardFileFilter;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
// import javax.servlet.http.HttpServlet;
// import javax.servlet.annotation.WebServlet;
// import servlets.UploadFileServlet;

public class WorkingFolderClass {

	// private static final long serialVersionUID = 1L;
	private static final Log LOG = LogFactory.getLog(WorkingFolderClass.class);
	private static String workingFolder = null;
	
	
	public WorkingFolderClass() throws WorkingFolderClassException {
		configureWorkingFolder();
	}
	public String getUserWorkingFolder(String localUserName) throws WorkingFolderClassException {

		configureWorkingFolder();
		String userWorkingFolder = workingFolder + fixlocalUserName(localUserName) + "/";
		testOrCreateWorkingFolder(userWorkingFolder);
		return userWorkingFolder ;
	}
	
	public String getWorkingFolder() throws WorkingFolderClassException {
		configureWorkingFolder();
		return workingFolder;
	}
	
	private String fixlocalUserName(String localUserName) throws WorkingFolderClassException {
		String fixedlocalUserName = null;
		if ((localUserName == null) || "".equals(localUserName)) {
			throw new WorkingFolderClassException("getWorkingFolder: localUserName can not be null nor empty string.");
		}
		else {
			fixedlocalUserName = localUserName.replaceAll("\\s", "_");
		}
		return fixedlocalUserName;
	}
		
	private void configureWorkingFolder() throws WorkingFolderClassException {
		if (isWorkingFolderinvalid()) {
			// Try the different options one by one.
			configureWorkingFolderAux("/home/java-apps/fuzzy-search/");
			configureWorkingFolderAux(System.getProperty("java.io.tmpdir") + "/java-apps/fuzzy-search/"); 
			// configureWorkingFolderAux(servlet.getServletContext().getInitParameter("working-folder-fuzzy-search"));
			configureWorkingFolderAux("/tmp/java-apps/fuzzy-search/");
			if (isWorkingFolderinvalid()) {
				throw new WorkingFolderClassException("configureWorkingFolder: Cannot configure any folder as working folder.");
			}
			else {
				LOG.info("choosen folder for uploads: " + workingFolder);
			}
		}
	}

	private void configureWorkingFolderAux(String newWorkingFolder) throws WorkingFolderClassException {
		LOG.info("configureWorkingFolderAux: testing: " + newWorkingFolder);
		
		if (isWorkingFolderinvalid()) {
			if (testOrCreateWorkingFolder(newWorkingFolder)) {
				workingFolder = newWorkingFolder;
			}
		}
	}

	private Boolean isWorkingFolderinvalid() {
		return ((workingFolder == null) || (workingFolder.equals("")));
	}
	
	private Boolean testOrCreateWorkingFolder(String newWorkingFolder) throws WorkingFolderClassException {
		boolean retval = false;
		
		if ((newWorkingFolder==null) || (newWorkingFolder.equals(""))){
			throw new WorkingFolderClassException("testOrCreateUserWorkingFolder: newWorkingFolder cannot be null nor empty string.");
		}
		else {
			
			String testFolder=newWorkingFolder+".test";
			File dir; 

			try {
				dir = new File(testFolder);
				if (dir.exists()) {
					dir.delete();
				}
				retval = dir.mkdirs();
			} 
			catch (Exception ex) {
				LOG.info("configureWorkingFolderAux: not valid: " + newWorkingFolder);
				LOG.info("Exception: " + ex);
			}
		}
		return retval;
	}
	
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public Boolean removeDataBase(String database, String localUserName) throws WorkingFolderClassException {
		configureWorkingFolder();
		String userWorkingFolder = getUserWorkingFolder(localUserName);
		
		String fileToRemove=userWorkingFolder+database;
		File dir;
		Boolean retval = false;
		
		try {
			dir = new File(fileToRemove);
			if (dir.exists()) {
				dir.delete();
				retval = true;
			}
		} 
		catch (Exception ex) {
			LOG.info("configureWorkingFolderAux: not valid: " + userWorkingFolder);
			LOG.info("Exception: " + ex);
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
	
	public ArrayList<DataBaseInfoClass> listDatabases(String localUserName) throws WorkingFolderClassException {
		
		configureWorkingFolder();		
		File dir = new File(workingFolder);
		ArrayList<DataBaseInfoClass> currentList = new ArrayList<DataBaseInfoClass>();

		FilenameFilter filter;
		String[] subDirs;
		
		filter = (FilenameFilter) new OnlyLocalUserNameFolderFilterClass(fixlocalUserName(localUserName));
		subDirs = dir.list(filter);

		if (subDirs != null) {
		    for (int i=0; i<subDirs.length; i++) {
		        // Get filename of file or directory
		    	currentList= listDatabasesInSubDir(subDirs[i], currentList);
		    }
		}

		filter = (FilenameFilter) new OnlyNotLocalUserNameFolderFilterClass(fixlocalUserName(localUserName));
		subDirs = dir.list(filter);

		if (subDirs != null) {
		    for (int i=0; i<subDirs.length; i++) {
		        // Get filename of file or directory
		    	currentList= listDatabasesInSubDir(subDirs[i], currentList);
		    }
		}
		
		return currentList;
	}

	private ArrayList<DataBaseInfoClass> listDatabasesInSubDir(String subDir, ArrayList<DataBaseInfoClass> currentList) throws WorkingFolderClassException {

		configureWorkingFolder();
		if ((subDir == null) || ("".equals(subDir))) {
			throw new WorkingFolderClassException("listDatabasesInSubDir: subDir cannot be null nor empty string.");
		}
		String realPathSubDir = workingFolder + subDir + "/";
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
	
	public static String lookForPlServer() throws WorkingFolderClassException {
		String pathOfPlServer = null;
		pathOfPlServer = lookForPlServerAux(pathOfPlServer, "/usr/lib/ciao/ciao-1.15/library/javall/plserver");
		pathOfPlServer = lookForPlServerAux(pathOfPlServer, "/home/vpablos/secured/CiaoDE_trunk/ciao/library/javall/plserver");
		pathOfPlServer = lookForPlServerAux(pathOfPlServer, "/home/vpablos/tmp/ciao-prolog-1.15.0+r14854/ciao/library/javall/plserver");
		pathOfPlServer = lookForPlServerAux(pathOfPlServer, "/home/tomcat/ciao-prolog-1.15.0+r14854/ciao/library/javall/plserver");
		
		// ToDo: Convendria un mecanismo algo más avanzado ... :-(
		lookForPlServerAdvanced(pathOfPlServer, "/usr/");
		lookForPlServerAdvanced(pathOfPlServer, "/home/");
		lookForPlServerAdvanced(pathOfPlServer, "/");
		
		if (pathOfPlServer == null) {
			throw new WorkingFolderClassException("lookForPlServer: impossible to find it.");
		}
		
		return pathOfPlServer;
	}
	
	private static String lookForPlServerAdvanced(String pathOfPlServer, String folderPath) {
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
	
	private static String lookForPlServerAux(String pathOfPlServer, String executableFileWithPath) {
		if (pathOfPlServer == null) {
			File file = new File(executableFileWithPath);
			if (file.exists() && file.isFile() && file.canRead() && file.canExecute()) {
				pathOfPlServer = executableFileWithPath;
			}
		}
		return pathOfPlServer;
	}	
	
}
