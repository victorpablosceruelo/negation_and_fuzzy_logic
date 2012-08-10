package auxiliar;


import java.io.File;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import javax.servlet.http.HttpServlet;
// import javax.servlet.annotation.WebServlet;
// import servlets.UploadFileServlet;

public class WorkingFolderClass {

	// private static final long serialVersionUID = 1L;
	final Log LOG = LogFactory.getLog(WorkingFolderClass.class);
	private static String workingFolder = null;
	
	
	public WorkingFolderClass(HttpServlet servlet) {
		configureWorkingFolder(servlet);
	}
	
	public String getWorkingFolder(HttpServlet servlet) {
		if ((workingFolder == null) || ("".equals(workingFolder))) {
			configureWorkingFolder(servlet);
		}
		return workingFolder;
	}
	
	private void configureWorkingFolder(HttpServlet servlet) {
		// Try the different options one by one.
		testWorkingFolder("/home/java-apps/fuzzy-search/");
		testWorkingFolder(System.getProperty("java.io.tmpdir") + "/java-apps/fuzzy-search/"); 
		testWorkingFolder(servlet.getServletContext().getInitParameter("working-folder-fuzzy-search"));
		testWorkingFolder("/tmp/java-apps/fuzzy-search/");
		LOG.info("choosen folder for uploads: " + workingFolder);
	}
	
	private void testWorkingFolder(String newWorkingFolder) {
		LOG.info("testworkingFolder: testing: " + newWorkingFolder);
		boolean retval = false;
		
		if ((workingFolder==null) || (workingFolder.equals(""))) {
			if ((newWorkingFolder!=null) && (! newWorkingFolder.equals(""))){

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
					LOG.info("testworkingFolder: not valid: " + newWorkingFolder);
					LOG.info("Exception: " + ex);
				}
				if (retval) {
					workingFolder = newWorkingFolder;
				}
			}
		}
	}

	
}
