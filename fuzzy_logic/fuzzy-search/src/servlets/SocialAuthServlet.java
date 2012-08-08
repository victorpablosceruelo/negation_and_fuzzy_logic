
package servlets;

import java.io.IOException;
import java.util.List;
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
import org.brickred.socialauth.AuthProvider;
import org.brickred.socialauth.Contact;
import org.brickred.socialauth.Profile;
import org.brickred.socialauth.SocialAuthConfig;
import org.brickred.socialauth.SocialAuthManager;
import org.brickred.socialauth.util.SocialAuthUtil;

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

@WebServlet("/")
public class SocialAuthServlet extends HttpServlet {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	final Log LOG = LogFactory.getLog(SocialAuthServlet.class);

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
			LOG.info("doGet invocation. Calling doPost");
			doPost(request, response);
	}
	
	public void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, java.io.IOException {
		
		
		try {
			socialAuthentication(request, response);
		} catch (Exception e) {
			LOG.error("Exception thrown: ");
			LOG.error(e);
			e.printStackTrace();
			ServletsAuxMethodsClass.goToAuthenticationSignout(request, response, LOG);
		}
	}
		
	private void socialAuthentication(HttpServletRequest request, HttpServletResponse response)
			throws Exception {
		
		LOG.info("socialAuthentication method call. ");
		// AuxMethodsClass.log_request_parameters(request, LOG);
		
		String request_mode = request.getParameter("mode");
		if ((request_mode == null) || ("".equals(request_mode))	) { 
			LOG.info("ERROR: erroneous request_mode (empty or null). ");
			request_mode = "signout";
		}

		if ("signed".equals(request_mode)) {
			socialAuthenticationSigned(request, response);
		}
		else {
			if ("signin".equals(request_mode)) {
				socialAuthenticationSignIn(request, response);
			}
			else {
				if ("signout".equals(request_mode)) {
					socialAuthenticationSignOut(request, response);
				}
				else {
					LOG.info("ERROR: erroneous request_mode: " + request_mode);
					socialAuthenticationSignOut(request, response);
				}
			}
		}
		
		LOG.info("Last line in doPost !!!");
	}
		
	private void socialAuthenticationSignIn(HttpServletRequest request, HttpServletResponse response) 
			throws Exception {
		
		LOG.info("socialAuthenticationSignIn method call. ");
		
		// Ask for the previously created session.
		HttpSession session = request.getSession(false);
		Boolean error = false;
		String msg = "";
		String athenticationUrl = "";
		
		if (session == null) {
			error = true;
			msg = "Session is null";
			LOG.info("ERROR: Session is null.");
		}
		else {
			// Get the value of the parameter; the name is case-sensitive
			String request_id = request.getParameter("id");

			if ((request_id == null) || ("".equals(request_id))) {
				// (request_id == null) :: The request parameter was not present in the query string. 
				// ("".equals(id)) The request parameter was present in the query string but has no value. 
				error = true;
				msg = "Session is null";
				LOG.info("ERROR: erroneous request_id (empty or null). ");
			}
			else {
				SocialAuthManager manager = (SocialAuthManager) session.getAttribute("authManager");
				if (manager == null) {
					LOG.info("INFO: creating a new manager because it was null. ");
					//Create an instance of SocialAuthConfgi object
					SocialAuthConfig config = SocialAuthConfig.getDefault();

					//load configuration. By default load the configuration from oauth_consumer.properties. 
					//You can also pass input stream, properties object or properties file name.
					config.load();

					//Create an instance of SocialAuthManager and set config
					manager = new SocialAuthManager();
					manager.setSocialAuthConfig(config);
				}
				else {
					LOG.info("INFO: using the manager stored in the session. ");
					if (request_id.equals((String)session.getAttribute("id"))) {
						LOG.info("INFO: the id is the expected one. ");
					}
					else {
						LOG.info("INFO: user is trying to authenticate with a different provider. ");
						error = true;
						msg = "You have tried to authenticate with a different provider when you were authenticated.";	
					}
				}

				if (! error) {
					// URL of YOUR application which will be called after authentication
					String appUrl = ServletsAuxMethodsClass.getAppUrlFromRequest(request, LOG);
					String successUrl= appUrl + "/SocialAuthServlet?mode=signed";

					// get Provider URL to which you should redirect for authentication.
					// id can have values "facebook", "twitter", "yahoo" etc. or the OpenID URL
					athenticationUrl = manager.getAuthenticationUrl(request_id, successUrl);

					// Store in session.
					session.setAttribute("authManager", manager);
					session.setAttribute("id", request_id);
					session.setAttribute("authenticated", false);

					if ((athenticationUrl == null) || ("".equals(athenticationUrl))) {
						LOG.info("ERROR: newUrl is empty or null !!! ");
						error = true;
						msg = "ERROR: newUrl is empty or null !!! ";	
					}
				}
			}
		}
		
		if (error) {
			request.setAttribute("msg1", msg);
			// ServletsAuxMethodsClass.goToAuthenticationSignout(request, response, LOG);
			socialAuthenticationSignOut(request, response);
		}
		else {
			LOG.info("Redirecting to: " + athenticationUrl);
			response.sendRedirect( athenticationUrl );
		}
	}
	
	private void socialAuthenticationSignOut(HttpServletRequest request, HttpServletResponse response) 
			throws ServletException, IOException {
		
		LOG.info("socialAuthenticationSignOut method call. ");
		
		// Ask for the previously created session.
		HttpSession session = request.getSession(false);

		if (session != null) {
			String id = (String)session.getAttribute("id");
			if (id != null) {
				SocialAuthManager manager = (SocialAuthManager) session.getAttribute("authManager");
				if (manager != null) {
					manager.disconnectProvider(id);
				}
			}
			session.invalidate();
		}
		ServletsAuxMethodsClass.goToAppIndex(request, response, LOG);
	}
	
	private void socialAuthenticationSigned(HttpServletRequest request, HttpServletResponse response) throws Exception {
		
		LOG.info("socialAuthenticationSigned method call. ");
		
		// Ask for the previously created session.
		HttpSession session = request.getSession(false);
		Boolean error = false;
		String msg = "";

		if (session == null) {
			error = true;
			msg = "ERROR: session is null.";
			LOG.info(msg);
		}
		else {
			// get the social auth manager from session
			SocialAuthManager manager = (SocialAuthManager)session.getAttribute("authManager");

			if (manager == null) {
				error = true;
				msg = "ERROR: manager is null.";
				LOG.info(msg);
			}
			else {
				// call connect method of manager which returns the provider object. 
				// Pass request parameter map while calling connect method. 
				AuthProvider provider = manager.connect(SocialAuthUtil.getRequestParametersMap(request));

				if (provider == null) {
					error = true;
					msg = "ERROR: provider is null.";
					LOG.info(msg);
				}
				else {
					// get profile
					Profile profile = provider.getUserProfile();

					// you can obtain profile information
					// System.out.println(profile.getFirstName());

					// OR also obtain list of contacts
					List<Contact> contactsList = provider.getContactList();

					session.setAttribute("authenticated", true);
					session.setAttribute("profile", profile);
					session.setAttribute("contactsList", contactsList);
				}
			}
		}
		
		if (error) {
			request.setAttribute("msg1", msg);
			socialAuthenticationSignOut(request, response);
			// ServletsAuxMethodsClass.goToAuthenticationSignout(request, response, LOG);
		}
		else {
			ServletsAuxMethodsClass.goToSearchMenu(request, response, LOG);
		}
	}

}

