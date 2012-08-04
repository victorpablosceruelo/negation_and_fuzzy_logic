package auxiliar;

import java.io.File;
import javax.servlet.http.HttpServlet;
/*
import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
*/

public class StorageClass {
	
	public StorageClass () {
		
	}
	
	public enum ValidPaths {
		programs, usersDB
	}

	private static String filesPath = "";

	public StorageClass(HttpServlet servlet) {
		// Reconfigure only the first time.
		if (filesPath.equals("")) {
			// Try the different options one by one.
			test_files_path("/home/java-apps/fuzzy-search/");
			test_files_path(System.getProperty("java.io.tmpdir") + "/java-apps/fuzzy-search/"); 
			test_files_path(servlet.getServletContext().getInitParameter("file-upload"));
			test_files_path("/tmp/java-apps/fuzzy-search/");
		}
	}
	
	private void test_files_path(String tmp_filesPath) {
		boolean retval = false;
		if (filesPath.equals("")) {
			File dir; 
			try {
				dir = new File(filesPath);
				retval = dir.mkdirs();

				dir = new File(getPathAux(tmp_filesPath, ValidPaths.programs));
				retval = dir.mkdirs();
				
			} 
			catch (Exception ex) {
				System.out.println(ex);
			}
			if (retval) filesPath = tmp_filesPath;
		}
	}
	
	private String getPathAux(String absolutePath, ValidPaths request) {
		String retval = absolutePath + "unknown/";
		switch (request) {
		case usersDB: 
			retval = absolutePath + "usersDB.sqlite";
			break;
		case programs: 
			retval = absolutePath + "programs/";
			break;
		default:
			break;
		}
		return retval;
			
	}
		
	public String getPath(ValidPaths request) {
		return getPathAux(filesPath, request);
	}
	
}
