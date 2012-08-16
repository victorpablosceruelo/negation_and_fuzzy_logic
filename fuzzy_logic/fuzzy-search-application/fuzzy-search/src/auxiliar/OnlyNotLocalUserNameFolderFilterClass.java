package auxiliar;

import java.io.File;
import java.io.FilenameFilter;

public class OnlyNotLocalUserNameFolderFilterClass implements FilenameFilter {

	private String userName = "";
	
	public OnlyNotLocalUserNameFolderFilterClass(String newUserName) {
		super();
		userName = newUserName;
	}
	
	@Override
	public boolean accept(File dir, String name) {
		return (! name.equals(userName));
	}

}
