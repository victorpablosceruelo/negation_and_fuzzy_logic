package auxiliar;


import java.io.File;
import java.io.FilenameFilter;
import java.util.ArrayList;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
// import javax.servlet.http.HttpServlet;
// import javax.servlet.annotation.WebServlet;
// import servlets.UploadFileServlet;

public class WorkingFolderClass {

	// private static final long serialVersionUID = 1L;
	final Log LOG = LogFactory.getLog(WorkingFolderClass.class);
	private static String workingFolder = null;
	
	
	public WorkingFolderClass() throws WorkingFolderClassException {
		configureWorkingFolder();
	}
	public String getUserWorkingFolder(String userDisplayName) throws WorkingFolderClassException {

		configureWorkingFolder();
		String userWorkingFolder = workingFolder + fixUserDisplayName(userDisplayName) + "/";
		testOrCreateWorkingFolder(userWorkingFolder);
		return userWorkingFolder ;
	}
	
	public String getWorkingFolder() throws WorkingFolderClassException {
		configureWorkingFolder();
		return workingFolder;
	}
	
	private String fixUserDisplayName(String userDisplayName) throws WorkingFolderClassException {
		String fixedUserDisplayName = null;
		if ((userDisplayName == null) || "".equals(userDisplayName)) {
			throw new WorkingFolderClassException("getWorkingFolder: userDisplayName can not be null nor empty string.");
		}
		else {
			fixedUserDisplayName = userDisplayName.replaceAll("\\s", "_");
		}
		return fixedUserDisplayName;
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
	
	public Boolean removeDataBase(String database, String userDisplayName) throws WorkingFolderClassException {
		configureWorkingFolder();
		String userWorkingFolder = getUserWorkingFolder(userDisplayName);
		
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
	
	public ArrayList<DataBaseInfoClass> listDatabases(String userDisplayName) throws WorkingFolderClassException {
		
		configureWorkingFolder();		
		File dir = new File(workingFolder);
		ArrayList<DataBaseInfoClass> currentList = new ArrayList<DataBaseInfoClass>();

		FilenameFilter filter;
		String[] subDirs;
		
		filter = (FilenameFilter) new OnlyUserDisplayNameFolderFilterClass(fixUserDisplayName(userDisplayName));
		subDirs = dir.list(filter);

		if (subDirs != null) {
		    for (int i=0; i<subDirs.length; i++) {
		        // Get filename of file or directory
		    	currentList= listDatabasesInSubDir(subDirs[i], currentList);
		    }
		}

		filter = (FilenameFilter) new OnlyNotUserDisplayNameFolderFilterClass(fixUserDisplayName(userDisplayName));
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
}
