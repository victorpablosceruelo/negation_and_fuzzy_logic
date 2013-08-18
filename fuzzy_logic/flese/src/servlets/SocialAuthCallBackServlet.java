package servlets;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.brickred.socialauth.Permission;
import org.brickred.socialauth.SocialAuthConfig;
import org.brickred.socialauth.SocialAuthManager;

import storeHouse.SessionStoreHouse;
import auxiliar.LocalUserInfo;
import auxiliar.NextStep;
import auxiliar.ServletsAuxMethodsClass;
import constants.KConstants;
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

@WebServlet("/SocialAuthCallBackServlet")
public class SocialAuthCallBackServlet extends HttpServlet {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private static final Log LOG = LogFactory.getLog(SocialAuthCallBackServlet.class);

	public void doGet(HttpServletRequest request, HttpServletResponse response) {
		doGetAndDoPost("doGet", request, response);
	}

	public void doPost(HttpServletRequest request, HttpServletResponse response) {
		doGetAndDoPost("doPost", request, response);
	}

	private void doGetAndDoPost(String doAction, HttpServletRequest request, HttpServletResponse response) {
		LOG.info("--- " + doAction + " invocation ---");
		NextStep nextStep = null;

		try {
			// Sessions management.
			SessionStoreHouse sessionStoreHouse = new SessionStoreHouse(request, response, true);

			nextStep = socialAuthentication(sessionStoreHouse);
			nextStep.takeAction(request, response);

		} catch (Exception e) {
			// socialAuthenticationSignOut(request, response, session);
			ServletsAuxMethodsClass.actionOnException(KConstants.Pages.SignOutRequest, "", e, request, response, LOG);
		}
		LOG.info("--- " + doAction + " end ---");
	}

	private NextStep socialAuthentication(SessionStoreHouse sessionStoreHouse) throws Exception {

		// The parameter that tells us the operation.
		if ("".equals(sessionStoreHouse.getRequestOp())) {
			return socialAuthenticationAuthenticate(sessionStoreHouse);
		}

		if ("signout".equals(sessionStoreHouse.getRequestOp())) {
			sessionStoreHouse.setRequestOp("");
			return socialAuthenticationSignOut(sessionStoreHouse);
		}

		if ("signin".equals(sessionStoreHouse.getRequestOp())) {
			sessionStoreHouse.setRequestOp("");
			return socialAuthenticationSignInOrContinue(sessionStoreHouse);
		}

		return null;

	}

	private NextStep socialAuthenticationAuthenticate(SessionStoreHouse sessionStoreHouse) throws Exception {

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
		return new NextStep(NextStep.Constants.redirect_to, KConstants.Pages.SignInRequest, "&id=" + providerId);
	}

	private NextStep socialAuthenticationSignInOrContinue(SessionStoreHouse sessionStoreHouse) throws Exception {

		LOG.info("socialAuthenticationSignIn method call. ");

		// Get the value of the parameter; the name is case-sensitive

		// Test if we have signed in before and the session contains the info.
		try {
			@SuppressWarnings("unused")
			LocalUserInfo localUserName = new LocalUserInfo(sessionStoreHouse);
			return new NextStep(NextStep.Constants.forward_to, KConstants.Pages.SignedInAnswer, "");
		} catch (Exception e) {
		}

		// URL of YOUR application which will be called after authentication
		String requestUrl = sessionStoreHouse.getRequestUrlString();
		String serverName = sessionStoreHouse.getServerName();
		String nextURL = KConstants.Pages.SocialAuthenticationCallBackRequest.getFullUrl(requestUrl, serverName);

		// Returns the host name of the server to which the request was
		// sent.

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

		return new NextStep(NextStep.Constants.sendRedirect_to, KConstants.Pages.EmptyPage, nextURL);
		// response.sendRedirect(nextURL);
		// response.encodeRedirectURL( athenticationUrl );

	}

	private NextStep socialAuthenticationSignOut(SessionStoreHouse sessionStoreHouse) throws Exception {

		LOG.info("socialAuthenticationSignOut method call. ");

		sessionStoreHouse.invalidateSession();

		return new NextStep(NextStep.Constants.forward_to, KConstants.Pages.SignedOutAnswer, "");
	}

}

// // EOF

