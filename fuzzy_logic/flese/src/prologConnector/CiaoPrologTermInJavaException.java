package prologConnector;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class CiaoPrologTermInJavaException extends Exception {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	final Log LOG = LogFactory.getLog(CiaoPrologTermInJavaException .class);

	public CiaoPrologTermInJavaException(String msg) {
		super(msg);
	}
	
}
