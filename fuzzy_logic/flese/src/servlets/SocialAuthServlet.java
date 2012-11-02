
package servlets;

import java.io.IOException;
// import java.util.List;
//import java.io.PrintWriter;
//import java.util.ArrayList;
//import java.util.Enumeration;
//import java.util.List;
//import java.util.Map;
//import java.io.InputStream;
//import java.io.IOException;

import javax.servlet.ServletException;
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
// import org.brickred.socialauth.util.SocialAuthUtil;

import auxiliar.LocalUserNameFixesClass;
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

@WebServlet("/SocialAuthServlet")
public class SocialAuthServlet extends HttpServlet {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private static final Log LOG = LogFactory.getLog(SocialAuthServlet.class);

	/**
	 * creates a instance of the requested provider from AuthProviderFactory and
	 * calls the getLoginRedirectURL() method to find the URL which the user
	 * should be redirect to.
	 * 
	 * @param mapping
	 *            the action mapping
	 * @param form
	 *            the action form
	 * @param request
	 *            the http servlet request
	 * @param response
	 *            tc the http servlet response
	 * @return ActionForward where the action should flow
	 * @throws Exception
	 *             if an error occurs
	 */

	public void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, java.io.IOException {
		LOG.info("--- doGet invocation ---");
		doGetAndDoPost(request, response);
		LOG.info("--- doGet end ---");
	}
	
	public void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, java.io.IOException {
		LOG.info("--- doPost invocation ---");
		doGetAndDoPost(request, response);
		LOG.info("--- doPost end ---");	
	}
	
	private void doGetAndDoPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, java.io.IOException {
		try {
			socialAuthentication(request, response);
		} catch (Exception e) {
			LOG.error("Exception thrown: ");
			LOG.error(e);
			e.printStackTrace();
			ServletsAuxMethodsClass.addMessageToTheUser(request, e.getMessage(), LOG);
			ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.SocialAuthServletSignOut, request, response, LOG);
		}
	}
	
	private void socialAuthentication(HttpServletRequest request, HttpServletResponse response)
			throws Exception {
		
		LOG.info("socialAuthentication method call. ");
		// AuxMethodsClass.log_request_parameters(request, LOG);
		
		String request_op = request.getParameter("op");
		if (! (request_op == null)) request.removeAttribute("op");
		
		if ((request_op == null) || ("".equals(request_op))	) { 
			LOG.info("ERROR: erroneous request_op (empty or null). ");
			request_op = "signin";
		}
		
		if ("signout".equals(request_op)) socialAuthenticationSignOut(request, response);
		if ("userInfo".equals(request_op)) socialAuthenticationUserInfo(request, response);

		if ("signin".equals(request_op)) {
			if (! socialAuthenticationInTestingMode(request, response)) {
				socialAuthenticationSignIn(request, response);
			}
		}

		if ((! "signin".equals(request_op)) &&
			(! "signout".equals(request_op)) && (! "userInfo".equals(request_op))) {
			LOG.info("ERROR: erroneous request_op: " + request_op);
			socialAuthenticationSignOut(request, response);
		}
	}
		
	private void socialAuthenticationSignIn(HttpServletRequest request, HttpServletResponse response) 
			throws Exception {
		
		LOG.info("socialAuthenticationSignIn method call. ");
		
		// Ask for the previously created session.
		HttpSession session = request.getSession(false);
		Boolean error = false;
		String msg = "";
		String athenticationUrl = "";
		SocialAuthManager manager = null;
		
		if (session == null) {
			error = true;
			msg = "Session is null";
			LOG.info("ERROR: Session is null.");
		}
		else {
			// Get the value of the parameter; the name is case-sensitive
			String id = (String) request.getParameter("id");
			// Only if parameter does not contain the id we try with the one saved in session.
			if ((id == null) || ("".equals(id))) {
				id = (String) session.getAttribute("id");
			}
				
			if ((id == null) || ("".equals(id))) {
				// (id == null) :: The request parameter was not present in the query string. 
				// ("".equals(id)) The request parameter was present in the query string but has no value. 
				error = true;
				msg = "id is null or empty in request and in session.";
				LOG.info("ERROR: erroneous id (empty or null) in request and in session. ");
			}
			
			if (! error) {
				 manager = (SocialAuthManager) session.getAttribute("authManager");
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
			}

			if (! error) {
				// URL of YOUR application which will be called after authentication
				String successUrl = ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.SocialAuthCallBackServlet, request, LOG);
				LOG.info("successUrl: " + successUrl);

				// get Provider URL to which you should redirect for authentication.
				// id can have values "facebook", "twitter", "yahoo" etc. or the OpenID URL
				athenticationUrl = manager.getAuthenticationUrl(id, successUrl);

				// Store in session.
				session.setAttribute("authManager", manager);
				session.setAttribute("id", id);
				session.setAttribute("authenticated", false);

				if ((athenticationUrl == null) || ("".equals(athenticationUrl))) {
					LOG.info("ERROR: newUrl is empty or null !!! ");
					error = true;
					msg = "ERROR: newUrl is empty or null !!! ";	
				}
			}
		}
		
		if (error) {
			ServletsAuxMethodsClass.addMessageToTheUser(request, msg, LOG);
			// ServletsAuxMethodsClass.goToAuthenticationSignout(request, response, LOG);
			socialAuthenticationSignOut(request, response);
		}
		else {
			LOG.info("Redirecting to: " + athenticationUrl);
			response.sendRedirect( athenticationUrl );
			// response.encodeRedirectURL( athenticationUrl );
		}
	}
	
	private void socialAuthenticationSignOut(HttpServletRequest request, HttpServletResponse response) 
			throws ServletException, IOException {
		
		LOG.info("socialAuthenticationSignOut method call. ");
		
		// Ask for the previously created session.
		HttpSession session = request.getSession(false);
		String [] msgs;

		if (session != null) {
			String id = (String)session.getAttribute("id");
			if (id != null) {
				SocialAuthManager manager = (SocialAuthManager) session.getAttribute("authManager");
				if (manager != null) {
					manager.disconnectProvider(id);
				}
			}

			msgs = (String []) session.getAttribute("msgs");
			session.invalidate();
			session = request.getSession(true);
			session.setAttribute("msgs", msgs);
		}
		
		ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.SocialAuthSignInPage, request, response, LOG);
	}
	
	private Boolean socialAuthenticationInTestingMode(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
		// Ask for the previously created session.
		HttpSession session = request.getSession(false);
		Boolean retval = false;

	    // Returns the host name of the server to which the request was sent.
	    LOG.info("request.getServerName(): " + request.getServerName());
	    if ((request.getServerName() != null) && ("localhost".equals(request.getServerName()))) {
	    	if (session != null) {
	    		retval = true; // Fake authentication !!!
	    		session.setAttribute("authenticated", true);
	    		
				// Determine correct value for variable localUserName
				String localUserName = LocalUserNameFixesClass.getLocalUserName(null);
	    		session.setAttribute("localUserName", localUserName);
	    		ServletsAuxMethodsClass.addMessageToTheUser(request, "Social Authentication in Testing mode.", LOG);
	    		ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.FilesMgmtServlet, request, response, LOG);	
	    	}
		}
	    

	    return retval;
	}
	
	private void socialAuthenticationUserInfo(HttpServletRequest request, HttpServletResponse response) 
			throws ServletException, IOException {
		if (ServletsAuxMethodsClass.clientSessionIsAuthenticated(request, response, LOG)) {
			ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.SocialAuthUserInfoPage, request, response, LOG);
		}
		else {
			socialAuthenticationSignOut(request, response);
		}
	}
	
}




//// EOF


