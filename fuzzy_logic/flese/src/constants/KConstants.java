package constants;

public class KConstants {

	public static final String appPath = "flese/";
	public static final String servletName = "/Servlet";
	
	public static final class Managers {
		public static final String managerSuffix = "Manager";
	}

	public static class Values {
		public static final String True = "true";
		public static final String False = "false";
	}
	public static class Request {
		public static final String managerParam = "manager";
		public static final String operationParam = "op";
		public static final String fileNameParam = "fileName";
		public static final String fileOwnerParam = "fileOwner";
		public static final String providerId = "id";
		public static final String isAjaxParam = "ajax";
		
	}

	public static class PathsMgmt {
		public static final String[] programFilesValidPaths = { "/home/java-apps/fuzzy-search/",
				System.getProperty("java.io.tmpdir") + "/java-apps/fuzzy-search/",
				// servlet.getServletContext().getInitParameter("working-folder-fuzzy-search"),
				"/tmp/java-apps/fuzzy-search/" };

		public static final String[] plServerValidSubPaths = { "/home/tomcat/ciao-prolog-1.15.0+r14854/ciao/library/javall/plserver",
				"/usr/share/CiaoDE/ciao/library/javall/plserver", "/usr/lib/ciao", "/usr/share/CiaoDE", "/usr", "/opt", "/home", "/" };

		public static final String plServerProgramFileName = "plserver"; 
	}
	
	public static class Communications {
		public static final int BUFSIZE = 4096;
		public static final int maxFileSize = 50000 * 1024;
		public static final int maxMemSize = 50000 * 1024;
	}
	
	public static class CiaoPrologQuery {
		public static long maximumNumberOfRetries = 9223372036854775807L;
		public static long maximumNumberOfAnswers = 9223372036854775807L;
		
	}
	
	public static class PlConnectionsPool {
		public static int maxNumOfConnections = 10;
	}
	
	public static class QueryParams {
		public static String queryLinesCounter = "queryLinesCounter";
	}
}

// **********************************
// **********************************
// **********************************

