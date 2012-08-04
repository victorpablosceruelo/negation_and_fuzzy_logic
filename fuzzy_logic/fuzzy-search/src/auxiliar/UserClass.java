package auxiliar;


import org.hibernate.Session;

public class UserClass {
	
	private long id;
	private String userName;
	private String userPWD;
	private String userEmail;
	private String userFirstName;
	private String userLastName;

	// To access the session.
	
	Session session = null;
	HibernationSessionClass hibernationSessionClass = null;
	
	public UserClass() throws HibernationSessionClassException {
		hibernationSessionClass = new HibernationSessionClass(); 
		session = hibernationSessionClass.getSession();
	}

	public void createUsername (String Username, String Password) {
		
	}

}
