package managers;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.brickred.socialauth.Permission;
import org.brickred.socialauth.SocialAuthConfig;
import org.brickred.socialauth.SocialAuthManager;

import auxiliar.LocalUserInfo;
import auxiliar.NextStep;
import constants.KUrls;
// import org.apache.commons.lang.StringUtils;
// import org.brickred.socialauth.Profile;
// import org.brickred.socialauth.Contact;
// import org.brickred.socialauth.Profile;

/**
 * 
 * It redirects the browser to an appropriate URL which will be used for
 * authentication with the provider that has been set by clicking the icon. It
 * creates an instance of the requested provider from AuthProviderFactory and
 * calls the getLoginRedirectURL() method to find the URL which the user should
 * be redirect to.
 * 
 * @author tarunn@brickred.com
 * 
 */
// public class SocialAuthenticationAction extends Action {

public class AuthManager extends AbstractManager {

	public AuthManager() {
		super();
	}

	private static final Log LOG = LogFactory.getLog(AuthManager.class);

	public NextStep authenticate() throws Exception {

		LOG.info("socialAuthenticationAuthenticate method call. ");
		String providerId = "";

		if (sessionStoreHouse.appIsInTestingMode()) {
			sessionStoreHouse.addMessageForTheUser("INFO: Social Authentication in Testing mode.");
		} else {
			// get the social auth manager from session
			providerId = sessionStoreHouse.tryAuthenticationWithSocialAuthManager();
		}

		// Test if we have an username or not.
		@SuppressWarnings("unused")
		LocalUserInfo localUserName = new LocalUserInfo(sessionStoreHouse);

		sessionStoreHouse.addMessageForTheUser("Welcome to the FleSe application !!");
		return new NextStep(NextStep.Constants.redirect_to, KUrls.Auth.SignIn, "&id=" + providerId);
	}

	public NextStep signIn() throws Exception {

		LOG.info("socialAuthenticationSignIn method call. ");

		// Get the value of the parameter; the name is case-sensitive

		// Test if we have signed in before and the session contains the info.
		try {
			@SuppressWarnings("unused")
			LocalUserInfo localUserName = new LocalUserInfo(sessionStoreHouse);
			return new NextStep(NextStep.Constants.forward_to, KUrls.Auth.SignInPage, "");
		} catch (Exception e) {
		}

		// URL of YOUR application which will be called after authentication
		NextStep nextStep = new NextStep(NextStep.Constants.sendRedirect_to, KUrls.Auth.SocialAuthCallback, "");
		String nextURL = nextStep.getFullUrl(sessionStoreHouse.getRequest(), sessionStoreHouse.getResponse(), false);

		// Returns the host name of the server to which the request was
		// sent.

		String serverName = sessionStoreHouse.getRequest().getServerName();
		if ((serverName != null) && ("localhost".equals(serverName))) {
			LOG.info("request.getServerName(): " + serverName);
			sessionStoreHouse.setAppInTestingMode(true);
		} else {
			SocialAuthManager socialAuthManager = sessionStoreHouse.getSocialAuthManager();

			if (socialAuthManager == null) {
				// Create an instance of SocialAuthConfgi object
				SocialAuthConfig config = SocialAuthConfig.getDefault();
				// config.setApplicationProperties()
				// load configuration. By default load the configuration
				// from oauth_consumer.properties.
				// You can also pass input stream, properties object or
				// properties file name.
				config.load();

				// Create an instance of SocialAuthManager and set config
				socialAuthManager = new SocialAuthManager();
				socialAuthManager.setSocialAuthConfig(config);
			}

			// get Provider URL to which you should redirect for
			// authentication.
			// id can have values "facebook", "twitter", "yahoo" etc. or the
			// OpenID URL
			String providerId = sessionStoreHouse.getProviderId();
			socialAuthManager.setPermission(providerId, Permission.AUTHENTICATE_ONLY);
			nextURL = socialAuthManager.getAuthenticationUrl(providerId, nextURL);

			// Store in session.
			sessionStoreHouse.setSocialAuthManager(socialAuthManager);

		}

		if (nextURL == null)
			throw new Exception("nextURL is null.");
		if ("".equals(nextURL))
			throw new Exception("nextURL is empty string.");

		return new NextStep(NextStep.Constants.sendRedirect_to, KUrls.Pages.Empty, nextURL);
		// response.sendRedirect(nextURL);
		// response.encodeRedirectURL( athenticationUrl );

	}

	public NextStep signOut() throws Exception {

		LOG.info("socialAuthenticationSignOut method call. ");

		sessionStoreHouse.invalidateSession();

		return new NextStep(NextStep.Constants.forward_to, KUrls.Auth.SignOutPage, "");
	}

	public NextStep getExceptionPage() {
		NextStep nextStep = new NextStep(NextStep.Constants.forward_to, KUrls.Pages.Exception, "");
		return nextStep;
	}

	public NextStep byDefaultMethod() throws Exception {
		return authenticate();
	}

	public boolean createSessionIfNull() {
		return true;
	}

}

// // EOF

