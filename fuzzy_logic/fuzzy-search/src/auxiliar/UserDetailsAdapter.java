package auxiliar;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.GrantedAuthorityImpl;
import org.springframework.security.core.userdetails.User;

// @Service("userDetailsAdapter")
public class UserDetailsAdapter extends org.springframework.security.core.userdetails.User {
    /**
	 * 
	 */
	private static final long serialVersionUID = 1L;
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
