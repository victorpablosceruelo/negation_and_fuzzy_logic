package auxiliar;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class FleSeLogs {

	private static final Log LOG = LogFactory.getLog(FleSeLogs.class);

	public static void log(String message) {
		LOG.info(message);
	}
	
	public static void log(Exception message) {
		LOG.info(message);
	}
	
}
