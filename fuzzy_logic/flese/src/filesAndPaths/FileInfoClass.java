package filesAndPaths;

public class FileInfoClass {

	private String fileName = null;
	private String fileOwner = null;
	
	public FileInfoClass(String fileName, String fileOwner) throws FileInfoException {
		if (fileName == null)  {
			throw new FileInfoException("FileInfoClass constructor: fileName can not be null.");
		}
		if (fileOwner == null) {
			throw new FileInfoException("FileInfoClass constructor: fileOwner can not be null.");
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
	public Boolean canDeleteFile(String localUserName) {
		return fileOwner.equals(localUserName);
	}
}
