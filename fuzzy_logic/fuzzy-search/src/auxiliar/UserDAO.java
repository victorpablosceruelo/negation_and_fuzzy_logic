package auxiliar;

import java.util.List;


public interface UserDAO {

	public void saveUser(UserClass user);
	public List<UserClass> listUser();
}
