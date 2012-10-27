package auxiliar;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class FoldersUtilsClassException extends Exception {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	final Log LOG = LogFactory.getLog(FoldersUtilsClassException .class);

	public FoldersUtilsClassException(String msg) {
		super(msg);
	}
	
}
