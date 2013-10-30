package logs;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import auxiliar.Dates;
import constants.KConstants;
import filesAndPaths.FilesAndPathsException;
import filesAndPaths.PathsMgmt;
import filesAndPaths.PathsUtils;

public class LogsFiles {

	private String fileName = null;
	private String logsFolderPath = null;
	private String fileFullPath = null;
	private PathsMgmt pathsMgmt = null;
	boolean error = false;

	LogsFiles(String fileName) {
		if ((fileName == null) || ("".equals(fileName))) {
			fileName = KConstants.Application.LogsDefaultName + Dates.getCurrentDate();
		}
		this.fileName = fileName;

		determineFilePaths();

		if (fileFullPath != null) {
			if (!existsFile()) {
				createFile();
			}
		}
	}

	private boolean existsFile() {
		if ((fileFullPath == null) || ("".equals(fileFullPath))) {
			return false;
		}
		File file = new File(fileFullPath);
		boolean retVal = file.exists();
		return retVal;
	}

	private void determineFilePaths() {

		String programFilesPath;
		try {
			this.pathsMgmt = new PathsMgmt();
			programFilesPath = this.pathsMgmt.getProgramFilesPath();
		} catch (FilesAndPathsException e) {
			e.printStackTrace();
			this.pathsMgmt = null;
			programFilesPath = null;
			this.error = true;
		}

		if (programFilesPath != null) {
			this.logsFolderPath = PathsUtils.concatPathsStrings(programFilesPath, KConstants.Application.LogsFolder);
		} else {
			this.logsFolderPath = null;
			this.error = true;
		}

		if (this.logsFolderPath != null) {
			this.fileFullPath = PathsUtils.concatPathsStrings(this.logsFolderPath, fileName);
		} else {
			this.fileFullPath = null;
			this.error = true;
		}
	}

	private void createFile() {
		if (this.error) {
			return;
		}

		try {
			pathsMgmt.createFolder(this.logsFolderPath, false);
		} catch (FilesAndPathsException e) {
			e.printStackTrace();
			this.error = true;
		}

		createFileAux();

	}

	private void createFileAux() {
		if (this.error) {
			return;
		}

		File file = new File(this.fileFullPath);
		try {
			file.createNewFile();
		} catch (IOException e) {
			this.error = true;
		}

		writeLogos(file);
	}

	private void writeLogos(File file) {
		FileWriter fw;
		BufferedWriter bw;
		try {
			fw = new FileWriter(file.getAbsoluteFile());
			bw = new BufferedWriter(fw);
			bw.write("\n");
			bw.write("### - FleSe Application Logs -- " + fileName + " - ###");
			bw.write("\n");
			bw.write("\n");
			bw.close();
		} catch (IOException e) {
			e.printStackTrace();
			this.error = true;
		}
	}

	synchronized public boolean append(String msg) {
		if (this.error) {
			return this.error;
		}

		if ((msg == null) || ("".equals(msg))) {
			return this.error;
		}

		FileWriter fw;
		try {
			/* the true will append the new data */
			fw = new FileWriter(this.fileFullPath, true);
			/* appends the string to the file */
			fw.write(msg + "\n");
			/* closes the file */
			fw.close();
		} catch (IOException e) {
			this.error = true;
			e.printStackTrace();
		}
		return this.error;
	}

}

/*
 * 
 */

/* EOF */