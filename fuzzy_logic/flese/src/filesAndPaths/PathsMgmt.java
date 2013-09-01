package filesAndPaths;

import java.io.File;

import managers.FilesManagerAux;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import constants.KConstants;

public class PathsMgmt {

	private static final Log LOG = LogFactory.getLog(FilesManagerAux.class);

	private static String programFilesPath = null;
	private static String plServerPath = null;

	public PathsMgmt() throws PathsMgmtException {
		if (programFilesPath == null) {
			String tmpProgramFilesPath = determineProgramFilesValidPath(KConstants.PathsMgmt.programFilesValidPaths);
			setProgramFilesPath(tmpProgramFilesPath);
			LOG.info("programFilesPath: " + programFilesPath);
		}

		if (plServerPath == null) {
			String tmpPlServerPath = determinePlServerValidPath(KConstants.PathsMgmt.plServerValidSubPaths);
			setPlServerPath(tmpPlServerPath);
			LOG.info("plServerPath: " + plServerPath);
		}
	}

	public String getProgramFilesPath() throws PathsMgmtException {
		if (programFilesPath == null)
			throw new PathsMgmtException("programFilesPath cannot be null.");
		return programFilesPath;
	}

	public String getPlServerPath() throws PathsMgmtException {
		if (plServerPath == null)
			throw new PathsMgmtException("plServerPath cannot be null.");
		return plServerPath;
	}

	private synchronized void setProgramFilesPath(String tmpProgramFilesPath) throws PathsMgmtException {
		if (programFilesPath == null) {
			programFilesPath = tmpProgramFilesPath;
		}
	}

	private synchronized void setPlServerPath(String tmpPlServerPath) throws PathsMgmtException {
		if (programFilesPath == null) {
			plServerPath = tmpPlServerPath;
		}
	}

	/**
	 * Returns which one of the programFilesValidPaths is the adequate one. It
	 * is recommended not to run this method more than once.
	 * 
	 * @param programFilesValidPaths
	 *            is a list with the paths to test.
	 */
	private String determineProgramFilesValidPath(String[] programFilesValidPaths) throws PathsMgmtException {
		String programFilesValidPath = null;
		int index = 0;

		while (((programFilesValidPath == null) || ("".equals(programFilesValidPath))) && (index < programFilesValidPaths.length)) {
			LOG.info(programFilesValidPaths[index]);
			try {
				if (PathsUtils.testIfFolderExists(programFilesValidPaths[index], true)) {
					programFilesValidPath = programFilesValidPaths[index];
				}
			} catch (Exception e) {
				programFilesValidPath = null;
			}
			if (programFilesValidPath == null)
				index++;
		}

		if ((programFilesValidPath == null) || ("".equals(programFilesValidPath)))
			throw new PathsMgmtException("programFilesValidPath cannot be null.");
		return programFilesValidPath;
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	/**
	 * Looks which one of the proposed subPaths for the plServer is the correct
	 * one.
	 * 
	 * @param plServerValidSubPaths
	 *            are the new proposed subpaths for the plServer.
	 * @throws PathsMgmtException
	 *             when none is valid.
	 * 
	 */
	private String determinePlServerValidPath(String[] plServerValidSubPaths) throws PathsMgmtException {
		String plServerPath = null;
		int index = 0;

		while (((plServerPath == null) || ("".equals(plServerPath))) && (index < plServerValidSubPaths.length)) {
			LOG.info(plServerValidSubPaths[index]);

			plServerPath = lookForPlServerFileInSubDir(plServerValidSubPaths[index]);

			if (plServerPath == null)
				index++;
		}

		if (plServerPath == null) {
			throw new PathsMgmtException("plServerPath cannot be null.");
		}
		return plServerPath;
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	private String lookForPlServerFileInSubDir(String subPath) {
		String result = null;

		if (subPath != null) {
			File file = new File(subPath);

			if ((file.exists()) && (file.canRead()) || (file.canExecute())) {
				if (file.isFile()) {
					if (KConstants.PathsMgmt.plServerProgramFileName.equals(file.getName())) {
						result = subPath;
					}
				} else {
					if (file.isDirectory()) {
						File[] subFiles = file.listFiles();
						int index = 0;
						while ((result == null) && (index < subFiles.length)) {
							result = lookForPlServerFileInSubDir(subFiles[index].getAbsolutePath());
						}
					} else {
						LOG.info("Impossible to process path (not a file nor a directory): " + subPath);
					}
				}
			} else {
				LOG.info("Impossible to process path (not exists, not readable or not executable): " + subPath);
			}
		}
		return result;
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

}
