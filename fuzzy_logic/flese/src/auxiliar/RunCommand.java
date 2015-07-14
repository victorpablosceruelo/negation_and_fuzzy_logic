package auxiliar;

import java.io.IOException;

public class RunCommand {

	public static boolean run(String commandFullPath) {
		Runtime rt = Runtime.getRuntime();
		Process pr = null;
		try {
			pr = rt.exec(commandFullPath);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		if (pr != null)
			return true;
		return false;
	}
}
