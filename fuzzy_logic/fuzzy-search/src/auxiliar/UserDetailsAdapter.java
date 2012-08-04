package auxiliar;

@Service("userDetailsAdapter")
public class UserDetailsAdapter {   

    private Long id;

    org.springframework.security.core.userdetails.User buildUserFromUserEntity(User userEntity) {
        String username = userEntity.getUsername();
        String password = userEntity.getPassword();
        boolean enabled = userEntity.isEnabled();
        boolean accountNonExpired = true;
        boolean credentialsNonExpired = true;
        boolean accountNonLocked = true;

        Collection<GrantedAuthority> authorities = new ArrayList<GrantedAuthority>();
        for (String authority: userEntity.getAuthorities()) {

            authorities.add(new GrantedAuthorityImpl(authority));
        }

        this.id = userEntity.getId();

        org.springframework.security.core.userdetails.User user = new org.springframework.security.core.userdetails.User(username, password, enabled, accountNonExpired, credentialsNonExpired, accountNonLocked, authorities);
        return user;
    }

    public Long getId() {
        return id;
    }

}
