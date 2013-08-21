package constants;

public class KConstants {

	public static final String appPath = "flese/";

	public static class request {
		public static final String operationParam = "op";
		public static final String fileNameParam = "fileName";
		public static final String fileOwnerParam = "fileOwner";
		public static final String providerId = "id";
	}

	public static class pathsMgmt {
		public static final String[] programFilesValidPaths = { "/home/java-apps/fuzzy-search/",
				System.getProperty("java.io.tmpdir") + "/java-apps/fuzzy-search/",
				// servlet.getServletContext().getInitParameter("working-folder-fuzzy-search"),
				"/tmp/java-apps/fuzzy-search/" };

		public static final String[] plServerValidSubPaths = { "/home/tomcat/ciao-prolog-1.15.0+r14854/ciao/library/javall/plserver",
				"/usr/share/CiaoDE/ciao/library/javall/plserver", "/usr/lib/ciao", "/usr/share/CiaoDE", "/usr", "/opt", "/home", "/" };

		public static final String plServerProgramFileName = "plserver"; 
	}
	
	public static class communications {
		private static final int BUFSIZE = 4096;
		private static final int maxFileSize = 50000 * 1024;
		private static final int maxMemSize = 50000 * 1024;
	}
}
