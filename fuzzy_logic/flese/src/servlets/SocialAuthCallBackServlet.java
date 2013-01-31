
package servlets;

import java.util.Iterator;
import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

// import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.brickred.socialauth.AuthProvider;
import org.brickred.socialauth.Permission;
// import org.brickred.socialauth.Profile;
// import org.brickred.socialauth.Contact;
// import org.brickred.socialauth.Profile;
import org.brickred.socialauth.SocialAuthConfig;
import org.brickred.socialauth.SocialAuthManager;
import org.brickred.socialauth.util.SocialAuthUtil;

import auxiliar.LocalUserNameClass;
import auxiliar.ServletsAuxMethodsClass;



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
		LOG.info("--- "+doAction+" invocation ---");
		//	ServletsAuxMethodsClass.logRequestParameters(request, LOG);
		HttpSession session = null;
		
		try {
			// Ask for an existing session or create a new one.
			session = request.getSession(true);

			socialAuthentication(request, response, session);
		} catch (Exception e) {
			// socialAuthenticationSignOut(request, response, session);
			ServletsAuxMethodsClass.actionOnException(ServletsAuxMethodsClass.SignOutRequest, "", e, request, response, LOG);
		}
		LOG.info("--- "+doAction+" end ---");
	}
	
	private void socialAuthentication(HttpServletRequest request, HttpServletResponse response, HttpSession session) throws Exception {
		
		// The parameter that tells us the operation.
		String request_op = request.getParameter("op");
		if ((request_op != null) && (! "".equals(request_op)) &&
				(("signout".equals(request_op) || ("signin".equals(request_op))))) { 
			request.removeAttribute("op");
			if ("signout".equals(request_op)) { 
				socialAuthenticationSignOut(request, response, session);
			}

			if ("signin".equals(request_op)) { 
				socialAuthenticationSignInOrContinue(request, response, session);
			}
		}
		else {
			socialAuthenticationAuthenticate(request, response, session);
		}
	}
	
	private void socialAuthenticationAuthenticate(HttpServletRequest request, HttpServletResponse response, HttpSession session)
			throws Exception {
		
		LOG.info("socialAuthenticationAuthenticate method call. ");
		if (session == null) throw new Exception("Session is null");

		String id="none";
		String isInTestingMode = (String) session.getAttribute("testingMode");
		if ((isInTestingMode != null) && ("true".equals(isInTestingMode))) {
	    	ServletsAuxMethodsClass.addMessageToTheUser(request, "INFO: Social Authentication in Testing mode.", LOG);
		}
		else {
			// get the social auth manager from session
			SocialAuthManager authManager = (SocialAuthManager)session.getAttribute("authManager");
			if (authManager == null) throw new Exception("authManager is null");
			session.removeAttribute("authManager");

			// call connect method of manager which returns the provider object. 
			// Pass request parameter map while calling connect method.
			AuthProvider provider = null;
			try {
				provider = authManager.connect(SocialAuthUtil.getRequestParametersMap(request));
			}
			catch (Exception e) {
				LOG.info("Error connecting social authentication provider.");
				LOG.info(e);
				if (e != null) {
					LOG.info(e.getMessage());
					LOG.info(e.toString());
				}
				provider = null;
				throw new Exception("Impossible to connect with service provider.");
			}
			if (provider == null) throw new Exception("provider is null");
			
			// Retrieve the provider id to rebuild the initial query.
			// NO: authManager.getCurrentAuthProvider().getProviderId();
			id = provider.getProviderId();

			// Save new computed results in session.
			session.setAttribute("authManager", authManager);
			session.setAttribute("provider", provider);
		}
		
		// Test if we have an username or not.
		@SuppressWarnings("unused")
		LocalUserNameClass localUserName = new LocalUserNameClass(request, response);
				
		ServletsAuxMethodsClass.addMessageToTheUser(request, "Welcome to the FleSe application !!", LOG);
		ServletsAuxMethodsClass.redirect_to(ServletsAuxMethodsClass.SignInRequest, "&id=" + id, request, response, LOG);
	}
	
	private void socialAuthenticationSignInOrContinue(HttpServletRequest request, HttpServletResponse response, HttpSession session) 
			throws Exception {
		
		LOG.info("socialAuthenticationSignIn method call. ");
		if (session == null) throw new Exception("Session is null");

		// Get the value of the parameter; the name is case-sensitive
		String id = (String) request.getParameter("id");
		if ((id == null) || ("".equals(id))) throw new Exception("id is null");

		// Test if we have signed in before and the session contains the info.
		boolean userIsAuthenticated = false;
		try {
			@SuppressWarnings("unused")
			LocalUserNameClass localUserName = new LocalUserNameClass(request, response);
			userIsAuthenticated = true;
		}
		catch (Exception e) {
			userIsAuthenticated = false;
		}
		
		if (userIsAuthenticated) {
			ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.SignedInAnswer, "", request, response, LOG);
		}
		else {
			// URL of YOUR application which will be called after authentication
			String nextURL = ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.SocialAuthenticationCallBackRequest, request, LOG);;
			
		    // Returns the host name of the server to which the request was sent.
		    
		    if ((request.getServerName() != null) && ("localhost".equals(request.getServerName()))) {
		    	LOG.info("request.getServerName(): " + request.getServerName());
		    	session.setAttribute("testingMode", "true");
			} 
		    else {
				SocialAuthManager authManager = (SocialAuthManager) session.getAttribute("authManager");
				if (authManager != null) {
					session.removeAttribute("authManager");
				}
				else {
					LOG.info("INFO: creating a new auth manager because it was null. ");

					//Create an instance of SocialAuthConfgi object
					SocialAuthConfig config = SocialAuthConfig.getDefault();
					// config.setApplicationProperties()
					//load configuration. By default load the configuration from oauth_consumer.properties. 
					//You can also pass input stream, properties object or properties file name.
					config.load();

					//Create an instance of SocialAuthManager and set config
					authManager = new SocialAuthManager();
					authManager.setSocialAuthConfig(config);
				}

				// get Provider URL to which you should redirect for authentication.
				// id can have values "facebook", "twitter", "yahoo" etc. or the OpenID URL
				authManager.setPermission(id, Permission.AUTHENTICATE_ONLY);
				nextURL = authManager.getAuthenticationUrl(id, nextURL);

				// Store in session.
				session.setAttribute("authManager", authManager);

			}
		    
			if (nextURL == null) throw new Exception("nextURL is null."); 
			if ("".equals(nextURL)) throw new Exception("nextURL is empty string."); 

	    	response.sendRedirect( nextURL );
			// response.encodeRedirectURL( athenticationUrl );

		}
	}
	
	private void socialAuthenticationSignOut(HttpServletRequest request, HttpServletResponse response, HttpSession session) 
			throws Exception {
		
		LOG.info("socialAuthenticationSignOut method call. ");

		if (session != null) {
			SocialAuthManager authManager = (SocialAuthManager) session.getAttribute("authManager");
			if (authManager != null) {
				List <String> connectedProvidersIds = authManager.getConnectedProvidersIds();
				if (connectedProvidersIds != null) {
					Iterator <String> connectedProvidersIdsIterator = connectedProvidersIds.iterator();
					if (connectedProvidersIdsIterator != null) {
						while(connectedProvidersIdsIterator.hasNext()) {
							String id = connectedProvidersIdsIterator.next();
							if (id != null) {
								authManager.disconnectProvider(id);
							}
						}
					}
				}
			}
			// Invalidate the session.
			session.invalidate();
		}
		
		ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.SignedOutAnswer, "", request, response, LOG);
	}
	
}




//// EOF


