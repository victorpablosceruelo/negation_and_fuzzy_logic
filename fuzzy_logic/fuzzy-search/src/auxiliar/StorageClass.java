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
