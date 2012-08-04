package auxiliar;

import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.cfg.Configuration;

public class HibernationSessionClass {

	private static Session session = null;
	SessionFactory sessionFactory = null;
	
	public HibernationSessionClass () {
		private static SessionFactory sessionFactory;
		private static ServiceRegistry serviceRegistry;

		private static SessionFactory configureSessionFactory() throws
		HibernateException {
		    Configuration configuration = new Configuration();
		    configuration.configure();
		    serviceRegistry = new ServiceRegistryBuilder().applySettings(configuration.getProperties()).buildServiceRegistry();
		    sessionFactory = configuration.buildSessionFactory(serviceRegistry);
		    return sessionFactory;

		// deprecated:
		// sessionFactory = new Configuration().configure().buildSessionFactory();
		
		
	}
	
	public void openSession () throws HibernationSessionClassException {
		if (session != null) {
			throw new HibernationSessionClassException("Session was open (expected closed).");
		}
		session = sessionFactory.openSession();
	}
	
	public void closeSession () throws HibernationSessionClassException {
		if (session == null) {
			throw new HibernationSessionClassException("Session was closed (expected open).");
		}
		
		session.flush();
		session.close();
	}
	
	/*
	 * Just use the session to save the objects and flush. Examples: 
	 * session.save(contact);
	 * session.flush();
	 * */
	
	public Session getSession() throws HibernationSessionClassException {
		if (session == null) {
			throw new HibernationSessionClassException("Session was closed (expected open).");
		}
		return session;
	}
	
}

