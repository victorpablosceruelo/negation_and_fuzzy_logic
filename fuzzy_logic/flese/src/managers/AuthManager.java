package managers;

import java.util.Iterator;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.brickred.socialauth.AuthProvider;
import org.brickred.socialauth.Permission;
import org.brickred.socialauth.Profile;
import org.brickred.socialauth.SocialAuthConfig;
import org.brickred.socialauth.SocialAuthManager;
import org.brickred.socialauth.util.SocialAuthUtil;

import results.ResultsStoreHouseUtils;
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

	private static final Log LOG = LogFactory.getLog(AuthManager.class);

	public AuthManager() {
		super();
	}

	public NextStep getExceptionPage() {
		NextStep nextStep = new NextStep(KConstants.NextStep.forward_to, KUrls.Pages.Exception, "");
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

		LOG.info("socialAuthenticationAuthenticate method call. ");
		String providerId = "";

		if (requestStoreHouse.session.appIsInTestingMode()) {
			ResultsStoreHouseUtils.addMessage(requestStoreHouse, "INFO: Social Authentication in Testing mode.");
		} else {
			// get the social auth manager from session
			providerId = tryAuthenticationWithSocialAuthManager();
		}

		// Test if we have an username or not.
		@SuppressWarnings("unused")
		LocalUserInfo localUserName = new LocalUserInfo(requestStoreHouse);

		ResultsStoreHouseUtils.addMessage(requestStoreHouse, "Welcome to the FleSe application !!");
		setNextStep(new NextStep(KConstants.NextStep.redirect_to, KUrls.Auth.SignIn, "&id=" + providerId));
	}
	
	private String tryAuthenticationWithSocialAuthManager() throws Exception {
		SocialAuthManager socialAuthManager = requestStoreHouse.session.getSocialAuthManager();
		if (socialAuthManager == null)
			throw new Exception("Social Auth Manager is null");
		else
			requestStoreHouse.session.setSocialAuthManager(null);

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
		requestStoreHouse.session.setSocialAuthManager(socialAuthManager);
		requestStoreHouse.session.setAuthProvider(authProvider);
		requestStoreHouse.session.setProviderId(providerId);
		requestStoreHouse.session.setUserProfile(profile);

		return providerId;
	}
	

	public void signIn() throws Exception {

		LOG.info("socialAuthenticationSignIn method call. ");

		// Get the value of the parameter; the name is case-sensitive

		// Test if we have signed in before and the session contains the info.
		try {
			@SuppressWarnings("unused")
			LocalUserInfo localUserName = new LocalUserInfo(requestStoreHouse);
			setNextStep(new NextStep(KConstants.NextStep.forward_to, KUrls.Auth.SignInPage, ""));
		} catch (Exception e) {
		}

		// URL of YOUR application which will be called after authentication
		NextStep nextStep = new NextStep(KConstants.NextStep.sendRedirect_to, KUrls.Auth.SocialAuthCallback, "");
		String nextURL = nextStep.getFullUrl(requestStoreHouse.getRequest(), requestStoreHouse.getResponse(), false);

		// Returns the host name of the server to which the request was
		// sent.

		String serverName = requestStoreHouse.getRequest().getServerName();
		if ((serverName != null) && ("localhost".equals(serverName))) {
			LOG.info("request.getServerName(): " + serverName);
			requestStoreHouse.session.setAppInTestingMode(true);
		} else {
			SocialAuthManager socialAuthManager = requestStoreHouse.session.getSocialAuthManager();

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
			String providerId = requestStoreHouse.getProviderId();
			socialAuthManager.setPermission(providerId, Permission.AUTHENTICATE_ONLY);
			nextURL = socialAuthManager.getAuthenticationUrl(providerId, nextURL);

			// Store in session.
			requestStoreHouse.session.setSocialAuthManager(socialAuthManager);

		}

		if (nextURL == null)
			throw new Exception("nextURL is null.");
		if ("".equals(nextURL))
			throw new Exception("nextURL is empty string.");

		setNextStep(new NextStep(KConstants.NextStep.sendRedirect_to, KUrls.Pages.Empty, nextURL));
		// response.sendRedirect(nextURL);
		// response.encodeRedirectURL( athenticationUrl );

	}

	public void signOut() throws Exception {

		LOG.info("socialAuthenticationSignOut method call. ");

		invalidateSession();

		setNextStep(new NextStep(KConstants.NextStep.forward_to, KUrls.Auth.SignOutPage, ""));
	}

	
	private void invalidateSession() {

		if (! requestStoreHouse.session.isNull()) {
			SocialAuthManager authManager = requestStoreHouse.session.getSocialAuthManager();
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
			requestStoreHouse.session.invalidateSession();
		}

	}
}

// // EOF

