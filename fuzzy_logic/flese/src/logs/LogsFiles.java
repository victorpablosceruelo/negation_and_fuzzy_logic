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

	private boolean error = false;
	private BufferedWriter bw = null;

	LogsFiles(String fileName) {
		if ((fileName == null) || ("".equals(fileName))) {
			fileName = KConstants.Application.LogsDefaultName + Dates.getCurrentDate();
		}

		String fileFullPath = null;
		LogsFilesPaths logsFilesPaths = new LogsFilesPaths();
		String logsFolderPath = logsFilesPaths.getLogsFolderPath();

		if (logsFolderPath != null) {
			fileFullPath = PathsUtils.concatPathsStrings(logsFolderPath, fileName);
		} else {
			fileFullPath = null;
			this.error = true;
		}

		if (fileFullPath != null) {
			if (!existsFile(fileFullPath)) {
				createFile(fileFullPath, logsFolderPath);
			}
		}
	}
	
	protected void finalize() throws Throwable {
		  // Invoke the finalizer of our superclass
		  // We haven't discussed superclasses or this syntax yet
		  super.finalize();
		  
		  // Now get rid of the resources you do not need any more.
		  closeFile();
	}

	private boolean existsFile(String fileFullPath) {
		if ((fileFullPath == null) || ("".equals(fileFullPath))) {
			return false;
		}
		File file = new File(fileFullPath);
		boolean retVal = file.exists();
		return retVal;
	}

	private void createFile(String fileFullPath, String folderFullPath) {
		if (this.error) {
			return;
		}

		try {
			PathsMgmt pathsMgmt = new PathsMgmt();
			pathsMgmt.createFolder(folderFullPath, false);
		} catch (FilesAndPathsException e) {
			e.printStackTrace();
			this.error = true;
		}

		createFileAux(fileFullPath);

	}

	private void createFileAux(String fileFullPath) {
		if (this.error) {
			return;
		}

		File file = new File(fileFullPath);
		try {
			file.createNewFile();
		} catch (IOException e) {
			this.error = true;
		}

		openFile(file);
		writeLogos(file.getName());
	}

	private void openFile(File file) {
		FileWriter fw = null;
		try {
			fw = new FileWriter(file.getAbsoluteFile(), true);
		} catch (IOException e) {
			e.printStackTrace();
			fw = null;
			this.error = true;
		}
		if (fw != null) {
			this.bw = new BufferedWriter(fw);
		}
	}

	private void closeFile() {
		if (bw != null) {
			try {
				bw.close();
			} catch (IOException e) {
				this.error = true;
				e.printStackTrace();
			}
		}
	}

	private boolean basicTest() {
		return ((!this.error) && (this.bw != null));
	}

	private void writeLogos(String fileName) {
		if (basicTest()) {
			try {
				bw.write("\n");
				bw.write("### - FleSe Application Logs -- " + fileName + " - ###");
				bw.write("\n");
				bw.write("\n");
			} catch (IOException e) {
				this.error = true;
				e.printStackTrace();
			}
		}
	}

	synchronized public boolean append(String msg) {
		if (! basicTest()) {
			return true;
		}

		if ((msg == null) || ("".equals(msg))) {
			return false;
		}

		try {
			bw.write(msg);
			bw.write("\n");
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