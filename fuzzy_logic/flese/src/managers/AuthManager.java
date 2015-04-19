package managers;

import storeHouse.SessionStoreHouse;
import urls.ServerAndAppUrls;
import authProviders.AbstractAuthProvider;
import authProviders.AuthProviderInterface;
import authProviders.AuthenticationResult;
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
		} else {
			signOut();
		}
	}

	public void callsRegistry() {
		LocalUserInfo localUserInfo = LocalUserInfo.getLocalUserInfo(requestStoreHouse);
		if (localUserInfo != null) {
			setNextStep(new NextStep(KConstants.NextStep.forward_to, KUrls.Auth.CallsRegistryPage, ""));
		} else {
			signOut();
		}
	}

	public void authenticate() {

		SessionStoreHouse sessionStoreHouse = requestStoreHouse.getSession();

		if (sessionStoreHouse != null) {
			if (!sessionStoreHouse.appIsInTestingMode()) {
				// get the social auth manager from the session
				try {
					AuthProviderInterface authProvider = sessionStoreHouse.getAuthProvider();
					if (authProvider == null) {
						String authProviderId = requestStoreHouse.getAuthProviderId();
						authProvider = AbstractAuthProvider.getInstance(authProviderId);
					}
					authProvider.authenticationCallback(requestStoreHouse);
				} catch (Exception e) {
					e.printStackTrace();
				}
			}

			// Test if we have an username or not.
			LocalUserInfo localUserInfo = LocalUserInfo.getLocalUserInfo(requestStoreHouse);

			if (localUserInfo != null) {
				String providerId = requestStoreHouse.getAuthProviderId();

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
		String providerId = requestStoreHouse.getAuthProviderId();

		// Returns the host name of the server to which the request was sent.

		if (ServerAndAppUrls.isAppInTestingMode(requestStoreHouse.getRequest())) {
			requestStoreHouse.getSession().setAppInTestingMode(true);
			localUserInfo = LocalUserInfo.getLocalUserInfo(requestStoreHouse);
			if (localUserInfo != null) {
				requestStoreHouse.getSession().setAuthProviderId("localhost");
				setNextStep(nextStep);
				return;
			}
		}

		SessionStoreHouse sessionStoreHouse = requestStoreHouse.getSession();
		AuthProviderInterface authProvider = sessionStoreHouse.getAuthProvider();
		AuthenticationResult authResult = null;
		if (authProvider == null) {
			signOut();
		} else {
			try {
				authResult = authProvider.authenticationFirstStep();
			} catch (Exception e) {
				authResult = null;
			}
		}

		if (authResult != null) {
			String[] authResultMsgs = authResult.getMessages();
			if (authResultMsgs.length > 0) {
				for (String msg : authResultMsgs) {
					resultsStoreHouse.addResultMessage(msg);
				}
			}

			if (authResult.getNextStep() != null) {
				// Store authentication provider in session.
				requestStoreHouse.getSession().setAuthProvider(authProvider);

				setNextStep(authResult.getNextStep());
				return;
			}

			// response.sendRedirect(nextURL);
			// response.encodeRedirectURL( athenticationUrl );
		}

		resultsStoreHouse.addResultMessage(KConstants.AppMsgs.errorTryingToAuthenticateUser);
		signOut();

	}

	public void signOut() {
		SessionStoreHouse sessionStoreHouse = requestStoreHouse.getSession();

		if (sessionStoreHouse != null) {
			AuthProviderInterface authProvider = sessionStoreHouse.getAuthProvider();
			if (authProvider != null) {
				authProvider.deauthenticate();
			}
		}

		sessionStoreHouse.setAuthProvider(null);
		sessionStoreHouse.setAuthProviderId(null);

		// Invalidate the session.
		sessionStoreHouse.invalidateSession();

		setNextStep(new NextStep(KConstants.NextStep.forward_to, KUrls.Auth.SignOutPage, ""));
	}
}

// // EOF

