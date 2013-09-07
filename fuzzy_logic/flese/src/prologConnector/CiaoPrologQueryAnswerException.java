package prologConnector;

public class CiaoPrologQueryAnswerException extends Exception {

	private static final long serialVersionUID = 1L;
	public CiaoPrologQueryAnswerException(String reason) {
		super(reason);
	}
}
