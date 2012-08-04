package auxiliar;

@Service("userDetailsService")
public class UserDetailsServiceImpl implements UserDetailsService {

    @Autowired
    private UserDao userDao;

    @Autowired
    private UserDetailsAdapter userDetailsAdapter;

    public UserDetails loadUserByUsername(String username) throws UsernameNotFoundException, DataAccessException {
        UserDetails userDetails = null;
        User userEntity = userDao.findByUsername(username);

        if (userEntity == null) {
          throw new UsernameNotFoundException("user not found");
        }
        userDetails = userDetailsAdapter.buildUserFromUserEntity(userEntity);

        return userDetails;
    }
}
