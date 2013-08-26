package filesAndPaths;

public class ProgramFileInfo {

	private String fileName = null;
	private String fileOwner = null;

	public ProgramFileInfo(String fileOwner, String fileName) throws FileInfoException {

		if (fileOwner == null) {
			throw new FileInfoException("FileInfoClass constructor: fileOwner can not be null.");
		}
		if ("".equals(fileOwner)) {
			throw new FileInfoException("FileInfoClass constructor: fileOwner can not be empty string.");
		}

		if (fileName == null) {
			throw new FileInfoException("FileInfoClass constructor: fileName can not be null.");
		}
		if ("".equals(fileName)) {
			throw new FileInfoException("FileInfoClass constructor: fileName can not be empty string.");
		}

		this.fileName = fileName;
		this.fileOwner = fileOwner;
	}

	public String getFileName() {
		return fileName;
	}

	public String getFileOwner() {
		return fileOwner;
	}

	public String getProgramFileFullPath() throws PathsMgmtException {
		PathsMgmt pathsMgmt = new PathsMgmt();
		String fullPath = pathsMgmt.getFullPathOfFile(this.fileOwner, this.fileName, false);
		return fullPath;
	}

	public String getProgramFileFolderFullPath() throws PathsMgmtException {
		PathsMgmt pathsMgmt = new PathsMgmt();
		String fullPath = pathsMgmt.getFullPathOfFile(this.fileOwner, "", false);
		return fullPath;
	}

	public Boolean canDeleteFile(String localUserName) {
		return fileOwner.equals(localUserName);
	}
}
