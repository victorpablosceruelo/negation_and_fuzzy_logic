
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
// import org.brickred.socialauth.AuthProvider;
// import org.brickred.socialauth.Contact;
// import org.brickred.socialauth.Profile;
import org.brickred.socialauth.SocialAuthConfig;
import org.brickred.socialauth.SocialAuthManager;
// import org.brickred.socialauth.util.Base64.InputStream;
// import org.brickred.socialauth.util.SocialAuthUtil;

import auxiliar.LocalUserNameClass;
import auxiliar.ServletsAuxMethodsClass;

@WebServlet("/SocialAuthServlet")
public class SocialAuthServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;
	private static final Log LOG = LogFactory.getLog(SocialAuthServlet.class);


	public void doGet(HttpServletRequest request, HttpServletResponse response) {
		doGetAndDoPost("doGet", request, response);
	}
	
	public void doPost(HttpServletRequest request, HttpServletResponse response) {
		doGetAndDoPost("doPost", request, response);
	}
	
	private void doGetAndDoPost(String doAction, HttpServletRequest request, HttpServletResponse response) {
		LOG.info("--- "+doAction+" invocation ---");
		try {
			socialAuthentication(request, response);
		} catch (Exception e) {
			ServletsAuxMethodsClass.actionOnException(ServletsAuxMethodsClass.SocialAuthServletSignOut, "", e, request, response, LOG);
		}
		LOG.info("--- "+doAction+" end ---");
	}
	
	private void socialAuthentication(HttpServletRequest request, HttpServletResponse response)
			throws Exception {
		
		LOG.info("socialAuthentication method call. ");
		// AuxMethodsClass.log_request_parameters(request, LOG);
		
		String request_op = request.getParameter("op");
		if (! (request_op == null)) request.removeAttribute("op");
		
		if ((request_op == null) || ("".equals(request_op))	|| (! "signin".equals(request_op))) { 
			LOG.info("ERROR: request_op has been changed to signout. ");
			request_op = "signout";
		}
		
		// Ask for an existing session.
		HttpSession session = request.getSession(false);
		
		// If we are not using signout we cannot have a session.
		if ((! "signout".equals(request_op)) && (session != null)) {
			socialAuthenticationSignOut(request, response, session);
			session = null;
		}
		
		
		if ("signout".equals(request_op)) 
			socialAuthenticationSignOut(request, response, session);
		else {
			// Ask for a new session.
			session = request.getSession(true);
			if (! socialAuthenticationInTestingMode(request, response, session)) {
				socialAuthenticationSignIn(request, response, session);
			}
		}
		
		if ("signout".equals(request_op)) {
			ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.SocialAuthSignInPage, "", request, response, LOG);
		}
	}

	private Boolean socialAuthenticationInTestingMode(HttpServletRequest request, HttpServletResponse response, HttpSession session) 
			throws Exception {
		// Ask for the previously created session.
		if (session == null) throw new Exception("session is null");
		Boolean retval = false;

	    // Returns the host name of the server to which the request was sent.
	    LOG.info("request.getServerName(): " + request.getServerName());
	    if ((request.getServerName() != null) && ("localhost".equals(request.getServerName()))) {
	    	
	    	retval = true; // Fake authentication !!!
	    	session.setAttribute("testingMode", "true");

	    	LocalUserNameClass localUserName = new LocalUserNameClass(request, response);
	    	// if (localUserName==null) throw new Exception("localUserName is null");
	    	session.setAttribute("localUserName", localUserName.getLocalUserName());

	    	ServletsAuxMethodsClass.addMessageToTheUser(request, "Social Authentication in Testing mode.", LOG);
	    	ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.FilesMgmtServlet, "", request, response, LOG);	
		} 

	    return retval;
	}
	
	private void socialAuthenticationSignIn(HttpServletRequest request, HttpServletResponse response, HttpSession session) 
			throws Exception {
		
		LOG.info("socialAuthenticationSignIn method call. ");		
		if (session == null) throw new Exception("Session is null");

		// Get the value of the parameter; the name is case-sensitive
		String id = (String) request.getParameter("id");
		if ((id == null) || ("".equals(id))) throw new Exception("id is null");

		SocialAuthManager manager = (SocialAuthManager) session.getAttribute("authManager");
		if (manager == null) {
			LOG.info("INFO: creating a new manager because it was null. ");
		
			//Create an instance of SocialAuthConfgi object
			SocialAuthConfig config = SocialAuthConfig.getDefault();
			// config.setApplicationProperties()
			//load configuration. By default load the configuration from oauth_consumer.properties. 
			//You can also pass input stream, properties object or properties file name.
			config.load();

			//Create an instance of SocialAuthManager and set config
			manager = new SocialAuthManager();
			manager.setSocialAuthConfig(config);
		}

		// URL of YOUR application which will be called after authentication
		String successUrl = ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.SocialAuthCallBackServlet, request, LOG);

		// get Provider URL to which you should redirect for authentication.
		// id can have values "facebook", "twitter", "yahoo" etc. or the OpenID URL
		String athenticationUrl = manager.getAuthenticationUrl(id, successUrl);

		// Store in session.
		session.setAttribute("authManager", manager);

		if (athenticationUrl == null) throw new Exception("athenticationUrl is null."); 
		if ("".equals(athenticationUrl)) throw new Exception("athenticationUrl is empty string."); 
		
		response.sendRedirect( athenticationUrl );
		// response.encodeRedirectURL( athenticationUrl );
	}
	
	private void socialAuthenticationSignOut(HttpServletRequest request, HttpServletResponse response, HttpSession session) 
			throws Exception {
		
		LOG.info("socialAuthenticationSignOut method call. ");

		if (session != null) {
			SocialAuthManager manager = (SocialAuthManager) session.getAttribute("authManager");
			if (manager != null) {
				List <String> connectedProvidersIds = manager.getConnectedProvidersIds();
				if (connectedProvidersIds != null) {
					Iterator <String> connectedProvidersIdsIterator = connectedProvidersIds.iterator();
					if (connectedProvidersIdsIterator != null) {
						while(connectedProvidersIdsIterator.hasNext()) {
							String id = connectedProvidersIdsIterator.next();
							if (id != null) {
								manager.disconnectProvider(id);
							}
						}
					}
				}
			}
			// Invalidate the session.
			session.invalidate();
		}
	}
	
}




//// EOF


