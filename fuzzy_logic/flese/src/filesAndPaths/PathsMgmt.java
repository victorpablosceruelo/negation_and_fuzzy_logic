package filesAndPaths;

import java.io.File;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import auxiliar.LocalUserInfo;
import auxiliar.LocalUserInfoException;
import constants.KConstants;

public class PathsMgmt {

	private static final Log LOG = LogFactory.getLog(FilesMgmt.class);

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

	public String getProgramFilesPath() {
		return programFilesPath;
	}

	public String getPlServerPath() {
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
				if (testIfFolderExists(programFilesValidPaths[index], true)) {
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

	/**
	 * Obtains the complete path of the program file.
	 * 
	 * @param fileOwner
	 *            It is the owner of the program file.
	 * @param fileName
	 *            It is the name of the file for which we are computing the
	 *            path.
	 * @param createFolderIfDoesNotExist
	 *            allows to create the folder if it does not exist.
	 * @return the complete path for the fileName (if it is not null) or for the
	 *         fileOwner.
	 * @exception PathsMgmtException
	 *                if programFilesPath is null, if fileOwner is null or if
	 *                the program file does not exist or is invalid.
	 * @throws LocalUserInfoException
	 */
	public String getFullPathOfFile(String fileOwner, String fileName, Boolean createFolderIfDoesNotExist) throws PathsMgmtException {

		if ((programFilesPath == null) || ("".equals(programFilesPath))) {
			throw new PathsMgmtException("programFilesPath is empty string or null.");
		}

		if (fileOwner == null) {
			throw new PathsMgmtException("fileOwner cannot be null.");
		}
		if ("".equals(fileOwner)) {
			throw new PathsMgmtException("fileOwner cannot be empty string.");
		}

		if (fileName == null)
			fileName = "";

		String fullPath = null;
		try {
			LocalUserInfo.checkUserNameIsValid(fileOwner);
		} catch (LocalUserInfoException e) {
			e.printStackTrace();
			throw new PathsMgmtException(e.getMessage());
		}
		String subPath = concatSubPaths(programFilesPath, fileOwner);
		if (testIfFolderExists(subPath, createFolderIfDoesNotExist)) {
			if (!"".equals(fileName)) {
				fullPath = concatSubPaths(subPath, fileName);
				if (testIfFileExists(fullPath, true)) {
					return fullPath;
				} else
					return null;
			} else {
				return subPath;
			}
		} else
			return null;
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	private boolean testIfFolderExists(String folderName, boolean createFolderIfDoesNotExist) throws PathsMgmtException {
		boolean retVal = false;

		File dir = new File(folderName);
		if (dir.exists()) {
			if (dir.isDirectory() && dir.canRead() && dir.canWrite() && dir.canExecute()) {
				retVal = true;
			}
		} else {
			if (createFolderIfDoesNotExist) {
				try {
					retVal = dir.mkdirs();
				} catch (Exception ex) {
					throw new PathsMgmtException("The folder " + folderName + "can not be created.");
				}
			}
		}
		return retVal;
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public Boolean testIfFileExists(String subPath, boolean launchException) throws PathsMgmtException {

		if (subPath == null) {
			throw new PathsMgmtException("subPath cannot be null.");
		}

		if ("".equals(subPath)) {
			throw new PathsMgmtException("subPath cannot be empty string.");
		}

		String fullPath = concatSubPaths(programFilesPath, subPath);
		return testIfFileExistsAux(fullPath, launchException);

	}

	private boolean testIfFileExistsAux(String fullPath, boolean launchException) throws PathsMgmtException {
		if (fullPath == null)
			throw new PathsMgmtException("fullPath cannot be null.");
		if ("".equals(fullPath))
			throw new PathsMgmtException("fullPath cannot be empty string.");
		if ("/".equals(fullPath))
			throw new PathsMgmtException("fullPath cannot be the string /.");

		File file = new File(fullPath);
		if (!file.exists()) {
			if (launchException)
				throw new PathsMgmtException("file does not exist. file: " + fullPath);
			return false;
		}
		if (!file.isFile()) {
			if (launchException)
				throw new PathsMgmtException("file is not a file. file: " + fullPath);
			return false;
		}
		if (!file.canRead()) {
			if (launchException)
				throw new PathsMgmtException("file is not readable. file: " + fullPath);
			return false;
		}
		return true;
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public static String concatSubPaths(String head, String tail) {
		String result = null;
		if (head.endsWith("/")) {
			if (tail.startsWith("/")) {
				result = head + tail.substring(1, tail.length() - 1);
			} else {
				result = head + tail;
			}
		} else {
			if (tail.startsWith("/")) {
				result = head + tail;
			} else {
				result = head + "/" + tail;
			}
		}

		return result;
	}
}
