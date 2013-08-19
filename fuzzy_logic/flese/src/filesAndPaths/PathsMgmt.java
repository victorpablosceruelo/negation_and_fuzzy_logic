package filesAndPaths;

import java.io.File;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import constants.KConstants;

public class PathsMgmt {

	private static String programFilesPath = null;
	private static final Log LOG = LogFactory.getLog(FilesMgmt.class);
	
	public PathsMgmt () throws Exception {
		if (programFilesPath == null) {
			String tmpProgramFilesPath = determineProgramFilesValidPath(KConstants.filesMgmt.programFilesValidPaths); 
			setProgramFilesPath(tmpProgramFilesPath);
			LOG.info("programFilesPath: " + programFilesPath);
		}
	}
	
	private synchronized void setProgramFilesPath (String tmpProgramFilesPath) throws Exception {
		if (programFilesPath == null) {
		programFilesPath = tmpProgramFilesPath;
		}
	}
	
	/**
	 * Returns which one of the programFilesValidPaths is the adequate one.
	 * It is recommended not to run this method more than once.
	 * 
	 * @param programFilesValidPaths is a list with the paths to test.
	 */
	private String determineProgramFilesValidPath(String [] programFilesValidPaths) throws Exception {
		String programFilesValidPath = null;
		int index = 0;
				
		while (((programFilesValidPath == null) || ("".equals(programFilesValidPath))) && 
				(index < programFilesValidPaths.length)) {
			LOG.info(programFilesValidPaths[index]);
			try {
				programFilesValidPath = getFullPath(programFilesValidPaths[index], null, null, true);
			}
			catch (Exception e) {
				programFilesValidPath = null;
			}
			if (programFilesValidPath == null) index++;
		}
		
		if ((programFilesValidPath == null) || ("".equals(programFilesValidPath))) 
			throw new Exception("programFilesValidPath cannot be null.");
		return programFilesValidPath;
	}
	
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////

	/**
	 * Looks which one of the proposed subPaths for the plServer is the correct one.
	 * 
	 * @param plServerValidSubPaths are the new proposed subpaths for the plServer.
	 * @throws Exception when none is valid.
	 * 
	 */
	public static String returnPlServerValidPath(String [] plServerValidSubPaths, Log LOG) throws Exception {
		String plServerPath = null;
		int index = 0;
		
		while (((plServerPath == null) || ("".equals(plServerPath))) &&
				(index < plServerValidSubPaths.length)) {
			LOG.info(plServerValidSubPaths[index]);
			
			plServerPath = lookForFileInSubDir("plserver", plServerValidSubPaths[index], LOG);
			
			if (plServerPath == null) index++;
		}
		
		if (plServerPath == null) {
			throw new Exception("plServerPath is null.");
		}
		return plServerPath;
	}
	
	private static String lookForFileInSubDir(String fileName, String subPath, Log LOG) {
		String result = null;
		
		if (subPath != null) {
			File file_1 = new File(subPath);
			
			if ((file_1.exists()) && (file_1.canRead()) || (file_1.canExecute())) {
				if (file_1.isFile()) {
					if (fileName.equals(file_1.getName())) {
						result = subPath;
					}
				}
				else {
					if (file_1.isDirectory()) {
						File[] subFiles = file_1.listFiles();
						int index = 0;
						while ((result == null) && (index < subFiles.length)) {
							result = lookForFileInSubDir(fileName, subFiles[index].getAbsolutePath(), LOG);
						}
					}
					else {
						LOG.info("Impossible to process path (not a file nor a directory): " + subPath);
					}
				}
			}
			else {
				LOG.info("Impossible to process path (not exists, not readable or not executable): " + subPath);
			}
		}
		return result;
	}	
	
	
}
