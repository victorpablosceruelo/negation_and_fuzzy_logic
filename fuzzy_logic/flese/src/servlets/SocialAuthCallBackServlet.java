
package servlets;

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
// import org.brickred.socialauth.Contact;
import org.brickred.socialauth.Profile;
import org.brickred.socialauth.SocialAuthManager;
import org.brickred.socialauth.util.SocialAuthUtil;

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

@WebServlet("/SocialAuthCallBackServlet")
public class SocialAuthCallBackServlet extends HttpServlet {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private static final Log LOG = LogFactory.getLog(SocialAuthCallBackServlet.class);

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
			socialAuthenticationAuthenticate(request, response);
		} catch (Exception e) {
			LOG.error("Exception thrown: ");
			LOG.error(e);
			e.printStackTrace();
			ServletsAuxMethodsClass.addMessageToTheUser(request, e.getMessage(), LOG);
			ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.SocialAuthServletSignOut, request, response, LOG);
		}
	}
	
	private void socialAuthenticationAuthenticate(HttpServletRequest request, HttpServletResponse response)
			throws Exception {
		
		LOG.info("socialAuthenticationAuthenticate method call. ");
		
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
					// List<Contact> contactsList = provider.getContactList();

					session.setAttribute("authenticated", true);
					session.setAttribute("profile", profile);
					session.setAttribute("provider", provider);

					// Determine correct value for variable localUserName
					String localUserName = LocalUserNameFixesClass.getLocalUserName(profile);
					session.setAttribute("localUserName", localUserName);
				}
			}
		}
		
		if (error) {
			ServletsAuxMethodsClass.addMessageToTheUser(request, msg, LOG);
			ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.SocialAuthServletSignOut, request, response, LOG);
		}
		else {
			ServletsAuxMethodsClass.addMessageToTheUser(request, "Welcome to the fuzzy search application !!", LOG);
			ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.FilesMgmtServlet, request, response, LOG);	
		}
	}	
}




//// EOF


