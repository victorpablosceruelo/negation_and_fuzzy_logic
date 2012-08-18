package auxiliar;

import java.io.File;
import java.io.FilenameFilter;

/**
 * Creates a filter to get only *.pl files.
 * 
 */
public class OnlyCiaoPrologFilesFilterClass implements FilenameFilter {

	@Override
	public boolean accept(File dir, String name) {
		return ((name.endsWith(".pl")) && (dir.isFile()) && (dir.canRead()));
	}

}
