package auxiliar;

@Service("userDetailsAdapter")
public class UserDetailsAdapter extends org.springframework.security.core.userdetails.User {
    private final Long id;
    public UserDetailsAdapter(User userEntity) {

        super(userEntity.getUsername(), userEntity.getPassword(), userEntity.isEnabled(), true, true, true, toAuthorities(userEntity.getAuthorities()));
        this.id = userEntity.getId();
    }

    private static Collection<GrantedAuthority> toAuthorities(List<String> authorities) {
        Collection<GrantedAuthority> authorityList = new ArrayList<GrantedAuthority>();
        for (String authority: authorities) {
            authorityList.add(new GrantedAuthorityImpl(authority));
        }
        return authorityList;
    }

    public Long getId() {
        return id;
    }

}
