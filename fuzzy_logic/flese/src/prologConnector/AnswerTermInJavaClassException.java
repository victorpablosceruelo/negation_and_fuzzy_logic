package prologConnector;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class AnswerTermInJavaClassException extends Exception {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	final Log LOG = LogFactory.getLog(AnswerTermInJavaClassException .class);

	public AnswerTermInJavaClassException(String msg) {
		super(msg);
	}
	
}
