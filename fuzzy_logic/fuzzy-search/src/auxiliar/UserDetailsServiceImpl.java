package auxiliar;

import org.springframework.security.core.userdetails.*;
import java.util.List;
import com.vaannila.dao.UserDAO;

@Service("userDetailsService")
public class UserDetailsServiceImpl implements UserDetailsService {

    // @Autowired
    private UserDao userDao;

    public UserDetails loadUserByUsername(String username) throws UsernameNotFoundException, DataAccessException {
        UserDetails userDetails = null;
        User userEntity = userDao.findByUsername(username);

        if (userEntity == null) {
          throw new UsernameNotFoundException("user not found");
        }
        userDetails = new UserDetailsAdapter(userEntity);

        return userDetails;
    }
}
