
package servlets;

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
// import org.brickred.socialauth.Profile;
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

	public void doGet(HttpServletRequest request, HttpServletResponse response) {
		LOG.info("--- doGet invocation ---");
		doGetAndDoPost(request, response);
		LOG.info("--- doGet end ---");
	}
	
	public void doPost(HttpServletRequest request, HttpServletResponse response) {
		LOG.info("--- doPost invocation ---");
		doGetAndDoPost(request, response);
		LOG.info("--- doPost end ---");	
	}
	
	private void doGetAndDoPost(HttpServletRequest request, HttpServletResponse response) {
		try {
			socialAuthenticationAuthenticate(request, response);
		} catch (Exception e) {
			ServletsAuxMethodsClass.actionOnException(e, request, response, LOG);
		}
	}
	
	private void socialAuthenticationAuthenticate(HttpServletRequest request, HttpServletResponse response)
			throws Exception {
		
		LOG.info("socialAuthenticationAuthenticate method call. ");
		
		// Ask for the previously created session.
		HttpSession session = request.getSession(true);

		if (session == null) throw new Exception("Session is null");

		// get the social auth manager from session
		SocialAuthManager manager = (SocialAuthManager)session.getAttribute("authManager");
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
			session.setAttribute("manager", manager);
		}

		// if (manager == null) throw new Exception("manager is null");
		
		// call connect method of manager which returns the provider object. 
		// Pass request parameter map while calling connect method. 
		AuthProvider provider = manager.connect(SocialAuthUtil.getRequestParametersMap(request));

		if (provider == null) throw new Exception("provider is null");
		session.setAttribute("provider", provider);
				
		ServletsAuxMethodsClass.addMessageToTheUser(request, "Welcome to the fuzzy search application !!", LOG);
		ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.FilesMgmtServlet, request, response, LOG);	
	}	
}




//// EOF


