package managers;

import java.util.Iterator;
import java.util.List;

import org.brickred.socialauth.AuthProvider;
import org.brickred.socialauth.Permission;
import org.brickred.socialauth.Profile;
import org.brickred.socialauth.SocialAuthConfig;
import org.brickred.socialauth.SocialAuthManager;
import org.brickred.socialauth.util.SocialAuthUtil;

import storeHouse.RequestStoreHouseException;
import auxiliar.LocalUserInfo;
import auxiliar.NextStep;
import constants.KConstants;
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

	public NextStep getExceptionPage() {
		NextStep nextStep = new NextStep(KConstants.NextStep.redirect_to, KUrls.Auth.SignOut, "");
		return nextStep;
	}

	public void byDefaultMethod() throws Exception {
		authenticate();
	}

	public boolean createSessionIfNull() {
		return true;
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public void authenticate() throws Exception {

		if (requestStoreHouse.getSession().appIsInTestingMode()) {
			resultsStoreHouse.addMessage("INFO: Social Authentication in Testing mode.");
		} else {
			// get the social auth manager from session
			tryAuthenticationWithSocialAuthManager();
		}

		// Test if we have an username or not.
		@SuppressWarnings("unused")
		LocalUserInfo localUserName = LocalUserInfo.getLocalUserInfo(requestStoreHouse);

		String providerId = requestStoreHouse.getProviderId();

		resultsStoreHouse.addMessage("Welcome to the FleSe application !!");
		setNextStep(new NextStep(KConstants.NextStep.redirect_to, KUrls.Auth.SignIn, "&id=" + providerId));
	}

	private void tryAuthenticationWithSocialAuthManager() throws Exception {
		SocialAuthManager socialAuthManager = requestStoreHouse.getSession().getSocialAuthManager();
		if (socialAuthManager == null)
			throw new Exception("Social Auth Manager is null");
		else
			requestStoreHouse.getSession().setSocialAuthManager(null);

		// call connect method of manager which returns the provider object.
		// Pass request parameter map while calling connect method.
		AuthProvider authProvider = null;
		try {
			authProvider = socialAuthManager.connect(SocialAuthUtil.getRequestParametersMap(requestStoreHouse.getRequest()));
		} catch (Exception e) {
			authProvider = null;
			e.printStackTrace();
			throw new Exception("Error connecting social authentication provider.");
		}

		if (authProvider == null)
			throw new Exception("provider is null");

		// Retrieve the provider id to rebuild the initial query.
		// NO: authManager.getCurrentAuthProvider().getProviderId();
		String providerId = authProvider.getProviderId();
		if (providerId == null)
			throw new Exception("providerId is null");

		Profile profile = authProvider.getUserProfile();
		if (profile == null)
			throw new Exception("profile is null");

		// Save new computed results in session.
		requestStoreHouse.getSession().setSocialAuthManager(socialAuthManager);
		requestStoreHouse.getSession().setAuthProvider(authProvider);
		requestStoreHouse.getSession().setProviderId(providerId);
		requestStoreHouse.getSession().setUserProfile(profile);
	}

	public void signIn() throws Exception {

		// URL of YOUR application which will be called after authentication
		NextStep nextStep = new NextStep(KConstants.NextStep.redirect_to, KUrls.Auth.SocialAuthCallback, "");
		String nextURL = nextStep.getUrl(true, true, false, requestStoreHouse.getRequest());

		// Test if we have signed in before and the session contains the info.
		// In that case we by-pass signIn and go directly to authentication.
		try {
			@SuppressWarnings("unused")
			LocalUserInfo localUserName = LocalUserInfo.getLocalUserInfo(requestStoreHouse);
			setNextStep(new NextStep(KConstants.NextStep.forward_to, KUrls.Auth.SignInPage, ""));
			return;
		} catch (Exception e) {
		}

		// Get the provider id.
		String providerId = requestStoreHouse.getProviderId();

		// Returns the host name of the server to which the request was sent.
		String serverName = requestStoreHouse.getRequest().getServerName();
		if ((serverName != null) && (("localhost".equals(serverName)) || KConstants.Application.inTestMode)) {
			requestStoreHouse.getSession().setAppInTestingMode(true);
			@SuppressWarnings("unused")
			LocalUserInfo localUserName = LocalUserInfo.getLocalUserInfo(requestStoreHouse);
			requestStoreHouse.getSession().setProviderId("localhost");
			setNextStep(nextStep);
		} else {
			SocialAuthManager socialAuthManager = requestStoreHouse.getSession().getSocialAuthManager();

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
			socialAuthManager.setPermission(providerId, Permission.AUTHENTICATE_ONLY);
			nextURL = socialAuthManager.getAuthenticationUrl(providerId, nextURL);

			// Store in session.
			requestStoreHouse.getSession().setSocialAuthManager(socialAuthManager);

			if (nextURL == null)
				throw new Exception("nextURL is null.");
			if ("".equals(nextURL))
				throw new Exception("nextURL is empty string.");

			setNextStep(new NextStep(KConstants.NextStep.redirect_to, null, nextURL));
		}

		// response.sendRedirect(nextURL);
		// response.encodeRedirectURL( athenticationUrl );

	}

	public void signOut() throws Exception {

		invalidateSession();
		setNextStep(new NextStep(KConstants.NextStep.forward_to, KUrls.Auth.SignOutPage, ""));
	}

	private void invalidateSession() throws RequestStoreHouseException {

		if (!requestStoreHouse.getSession().isNull()) {
			SocialAuthManager authManager = requestStoreHouse.getSession().getSocialAuthManager();
			if (authManager != null) {
				List<String> connectedProvidersIds = authManager.getConnectedProvidersIds();
				if (connectedProvidersIds != null) {
					Iterator<String> connectedProvidersIdsIterator = connectedProvidersIds.iterator();
					if (connectedProvidersIdsIterator != null) {
						while (connectedProvidersIdsIterator.hasNext()) {
							String id = connectedProvidersIdsIterator.next();
							if (id != null) {
								authManager.disconnectProvider(id);
							}
						}
					}
				}
			}
			// Invalidate the session.
			requestStoreHouse.getSession().invalidateSession();
		}

	}
}

// // EOF

