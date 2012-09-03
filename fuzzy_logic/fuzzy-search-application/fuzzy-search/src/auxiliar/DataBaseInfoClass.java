package auxiliar;

public class DataBaseInfoClass {

	private String dataBaseName = null;
	private String dataBaseOwner = null;
	
	public DataBaseInfoClass(String dataBaseName, String dataBaseOwner) throws FoldersUtilsClassException {
		if (dataBaseName == null)  {
			throw new FoldersUtilsClassException("DataBaseInfoClass constructor: dataBaseName can not be null.");
		}
		if (dataBaseOwner == null) {
			throw new FoldersUtilsClassException("DataBaseInfoClass constructor: dataBaseOwner can not be null.");
		}
		this.dataBaseName = dataBaseName;
		this.dataBaseOwner = dataBaseOwner;
	}
	
	public String getDataBaseName() {
		return dataBaseName;
	}
	public String getDataBaseOwner() {
		return dataBaseOwner;
	}
	public Boolean canDeleteDataBase(String localUserName) {
		return dataBaseOwner.equals(localUserName);
	}
}
