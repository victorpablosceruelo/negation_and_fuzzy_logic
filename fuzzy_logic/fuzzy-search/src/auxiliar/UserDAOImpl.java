package auxiliar;

import java.util.List;

import org.hibernate.Session;
import org.hibernate.Transaction;

// import com.googlecode.s2hibernate.struts2.plugin.annotations.SessionTarget;
// import com.googlecode.s2hibernate.struts2.plugin.annotations.TransactionTarget;

public class UserDAOImpl implements UserDAO {

	// @SessionTarget
	Session session;

	// @TransactionTarget
	Transaction transaction;

	@SuppressWarnings("unchecked")
	@Override
	public List<UserClass> listUser() {   
		List<UserClass> courses = null;
		try {
			courses = session.createQuery("from User").list();
		} catch (Exception e) {
			e.printStackTrace();
		}
		return courses;
	}

	@Override
	public void saveUser(UserClass user) {
		try {
			session.save(user);
		} catch (Exception e) {
			transaction.rollback();
			e.printStackTrace();
		}
	}

}