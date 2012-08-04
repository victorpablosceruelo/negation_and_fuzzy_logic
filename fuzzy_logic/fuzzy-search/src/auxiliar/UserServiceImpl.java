package auxiliar;

@Service
public class UserServiceImpl implements UserService {

    @Autowired
    private UserDao userDao;

    @Autowired
    private PasswordEncoder passwordEncoder;

    @Autowired
    private SaltSource saltSource;

    public User getByUsername(String username) {
        return userDao.findByUsername(username);
    }

    public User getByEmail(String email) {
        return userDao.findByEmail(email);
    }

    public void createUser(User user) {
        userDao.create(user);

        UserDetailsAdapter userDetails = new UserDetailsAdapter(user);
        String password = userDetails.getPassword();
        Object salt = saltSource.getSalt(userDetails);
        user.setPassword(passwordEncoder.encodePassword(password, salt));
        userDao.update(user);

    }

    public void updateUser(User user) {
        userDao.update(user);
    }
}
