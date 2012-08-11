package auxiliar;

import java.io.File;
import java.io.FilenameFilter;

public class OnlyCiaoPrologFilesFilterClass implements FilenameFilter {

	@Override
	public boolean accept(File dir, String name) {
		return name.endsWith(".pl");
	}

}
