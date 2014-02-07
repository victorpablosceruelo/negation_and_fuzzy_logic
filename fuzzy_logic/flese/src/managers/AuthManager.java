package managers;

import java.util.Iterator;
import java.util.List;

import org.brickred.socialauth.AuthProvider;
import org.brickred.socialauth.Permission;
import org.brickred.socialauth.Profile;
import org.brickred.socialauth.SocialAuthConfig;
import org.brickred.socialauth.SocialAuthManager;
import org.brickred.socialauth.util.SocialAuthUtil;

import storeHouse.SessionStoreHouse;
import urls.ServerAndAppUrls;
import auxiliar.LocalUserInfo;
import auxiliar.NextStep;
import constants.KConstants;
import constants.KUrls;

/**
 * 
 * It redirects the browser to an appropriate URL which will be used for
 * authentication with the provider that has been set by clicking the icon. It
 * creates an instance of the requested provider from AuthProviderFactory and
 * calls the getLoginRedirectURL() method to find the URL which the user should
 * be redirect to.
 * 
 */
// public class SocialAuthenticationAction extends Action {

public class AuthManager extends AbstractManager {
	// final Log LOG = LogFactory.getLog(AuthManager.class);

	public AuthManager() {
		super();
	}

	@Override
	public void actionWhenExceptionInTargetMethodInvocation(String methodName) {
		signOut();
	}

	@Override
	public String methodToInvokeIfMethodRequestedIsNotAvailable() {
		return "authenticate";
	}
	
	@Override
	public boolean createSessionIfNull() {
		return true;
	}

	@Override
	public boolean exceptionIfLocalUserInfoIsNull() {
		return false;
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public void providers() throws Exception {
		setNextStep(new NextStep(KConstants.NextStep.forward_to, KUrls.Auth.ProvidersPage, ""));
	}

	public void about() throws Exception {
		setNextStep(new NextStep(KConstants.NextStep.forward_to, KUrls.Auth.AboutPage, ""));
	}

	public void logs() throws Exception {
		LocalUserInfo localUserInfo = LocalUserInfo.getLocalUserInfo(requestStoreHouse);
		if (localUserInfo != null) {
			setNextStep(new NextStep(KConstants.NextStep.forward_to, KUrls.Auth.LogsPage, ""));
		}
		else {
			signOut();
		}
	}

	public void authenticate() {

		SessionStoreHouse sessionStoreHouse = requestStoreHouse.getSession();

		if (sessionStoreHouse != null) {
			if (!sessionStoreHouse.appIsInTestingMode()) {
				// get the social auth manager from the session
				try {
					tryAuthenticationWithSocialAuthManager();
				} catch (Exception e) {
					e.printStackTrace();
				}
			}

			// Test if we have an username or not.
			LocalUserInfo localUserInfo = LocalUserInfo.getLocalUserInfo(requestStoreHouse);

			if (localUserInfo != null) {
				String providerId = requestStoreHouse.getProviderId();

				if ((providerId != null) && (!"".equals(providerId))) {
					resultsStoreHouse.addResultMessage("Welcome to the FleSe application !!");
					setNextStep(new NextStep(KConstants.NextStep.redirect_to, KUrls.Auth.SignIn, "&id=" + providerId));
					return;
				}
			}
		}
		resultsStoreHouse.addResultMessage(KConstants.AppMsgs.errorTryingToAuthenticateUser);
		signOut();
	}

	private void tryAuthenticationWithSocialAuthManager() throws Exception {
		SocialAuthManager socialAuthManager = requestStoreHouse.getSession().getSocialAuthManager();

		// Re-Initialize in session. If an exception occurs this avoids
		// problems.
		requestStoreHouse.getSession().setSocialAuthManager(null);
		requestStoreHouse.getSession().setAuthProvider(null);
		requestStoreHouse.getSession().setProviderId(null);
		requestStoreHouse.getSession().setUserProfile(null);

		if (socialAuthManager == null) {
			throw new Exception("Social Auth Manager is null");
		}

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

	public void signIn() {

		// URL of YOUR application which will be called after authentication
		NextStep nextStep = new NextStep(KConstants.NextStep.redirect_to, KUrls.Auth.SocialAuthCallback, "");
		String nextURL = nextStep.getUrl(true, true, false, requestStoreHouse.getRequest());

		// Test if we have signed in before and the session contains the info.
		// In that case we by-pass signIn and go directly to authentication.
		LocalUserInfo localUserInfo = LocalUserInfo.getLocalUserInfo(requestStoreHouse);
		if (localUserInfo != null) {
			resultsStoreHouse.addResultMessage(KConstants.AppMsgs.welcomeToFleSe);
			setNextStep(new NextStep(KConstants.NextStep.forward_to, KUrls.Auth.SignInPage, ""));
			return;
		}

		// Get the provider id.
		String providerId = requestStoreHouse.getProviderId();

		// Returns the host name of the server to which the request was sent.

		if (ServerAndAppUrls.isAppInTestingMode(requestStoreHouse.getRequest())) {
			requestStoreHouse.getSession().setAppInTestingMode(true);
			localUserInfo = LocalUserInfo.getLocalUserInfo(requestStoreHouse);
			if (localUserInfo != null) {
				requestStoreHouse.getSession().setProviderId("localhost");
				setNextStep(nextStep);
				return;
			}
		}

		// Normal process.
		SocialAuthManager socialAuthManager = requestStoreHouse.getSession().getSocialAuthManager();

		if (socialAuthManager == null) {
			// Create an instance of SocialAuthConfgi object
			SocialAuthConfig config = SocialAuthConfig.getDefault();
			// config.setApplicationProperties()
			// load configuration. By default load the configuration from
			// oauth_consumer.properties.
			// You can also pass input stream, properties object or
			// properties file name.
			try {
				config.load();
			} catch (Exception e) {
				e.printStackTrace();
				config = null;
			}

			if (config != null) {
				// Create an instance of SocialAuthManager and set config
				socialAuthManager = new SocialAuthManager();
				try {
					socialAuthManager.setSocialAuthConfig(config);
				} catch (Exception e) {
					e.printStackTrace();
					socialAuthManager = null;
				}
			}
		}

		if (socialAuthManager != null) {
			// get Provider URL to which you should redirect for authentication.
			// id can have values "facebook", "twitter", "yahoo" etc. or the
			// OpenID URL
			socialAuthManager.setPermission(providerId, Permission.AUTHENTICATE_ONLY);
			try {
				nextURL = socialAuthManager.getAuthenticationUrl(providerId, nextURL);
			} catch (Exception e) {
				e.printStackTrace();
				nextURL = null;
				resultsStoreHouse.addResultMessage(e.getMessage());
			}

			if ((nextURL != null) && (!"".equals(nextURL))) {
				// Store social authentication manager in session.
				requestStoreHouse.getSession().setSocialAuthManager(socialAuthManager);

				setNextStep(new NextStep(KConstants.NextStep.redirect_to, null, nextURL));
				return;
			}

			// response.sendRedirect(nextURL);
			// response.encodeRedirectURL( athenticationUrl );
		}
		
		resultsStoreHouse.addResultMessage(KConstants.AppMsgs.errorTryingToAuthenticateUser);
		signOut();

	}

	public void signOut() {

		invalidateSession();
		setNextStep(new NextStep(KConstants.NextStep.forward_to, KUrls.Auth.SignOutPage, ""));
	}

	private void invalidateSession() {

		SessionStoreHouse sessionStoreHouse = requestStoreHouse.getSession();

		if ((sessionStoreHouse != null) && (!sessionStoreHouse.isNull())) {
			SocialAuthManager authManager = sessionStoreHouse.getSocialAuthManager();
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
			
			sessionStoreHouse.setSocialAuthManager(null);
			sessionStoreHouse.setAuthProvider(null);
			sessionStoreHouse.setProviderId(null);
			sessionStoreHouse.setUserProfile(null);

			
			// Invalidate the session.
			sessionStoreHouse.invalidateSession();
		}

	}
}

// // EOF

