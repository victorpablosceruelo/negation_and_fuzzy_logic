package auxiliar;

import java.io.File;
import java.io.FilenameFilter;

public class OnlyLocalUserNameFolderFilterClass implements FilenameFilter {

	private String userName = "";
	
	public OnlyLocalUserNameFolderFilterClass(String newUserName) {
		super();
		userName = newUserName;
	}
	
	@Override
	public boolean accept(File dir, String name) {
		return name.equals(userName);
	}

}
