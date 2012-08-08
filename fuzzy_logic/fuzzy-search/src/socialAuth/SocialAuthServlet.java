
package socialAuth;

// import java.io.IOException;
import java.io.InputStream;
//import java.io.PrintWriter;
//import java.util.ArrayList;
//import java.util.Enumeration;
//import java.util.List;
//import java.util.Map;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

// import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

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
		
		HttpSession session = request.getSession(true);
		LOG.info("socialAuthentication entered");
		// AuxMethodsClass.log_request_parameters(request, LOG);
		
		String request_mode = request.getParameter("mode");
		if ((request_mode == null) || ("".equals(request_mode))	) { 
			LOG.info("ERROR: erroneous request_mode (empty or null). ");
			ServletsAuxMethodsClass.goToAuthenticationSignout(request, response, LOG);
		}
	    else {
	    	if ("signin".equals(request_mode)) {
	    		socialAuthenticationSignIn(session, request, response);
	    	}
	    	else {
	    		if ("signout".equals(request_mode)) {
	    			socialAuthenticationSignOut(session, response);
	    			ServletsAuxMethodsClass.goToAppIndex(request, response, LOG);
	    		}
	    		else {
		    		if ("signed".equals(request_mode)) {
		    			socialAuthenticationSigned(session, response);
		    			ServletsAuxMethodsClass.goToAppIndex(request, response, LOG);
		    		}
		    		else {
		    			LOG.info("ERROR: erroneous request_mode: " + request_mode);
		    			ServletsAuxMethodsClass.goToAuthenticationSignout(request, response, LOG);
		    		}
	    		}
	    	}
	    }

		LOG.info("Last line in doPost !!!");
	}
		
	private void socialAuthenticationSignIn(HttpSession session, HttpServletRequest request, HttpServletResponse response) 
			throws Exception {
		// AuthForm authForm = (AuthForm) form;
		String appUrl = ServletsAuxMethodsClass.getAppUrlFromRequest(request, LOG);
		AuthForm authForm = null;
		String newUrl = "";

		// Get the value of the parameter; the name is case-sensitive
		String request_id = request.getParameter("id");

		if ((request_id == null) || ("".equals(request_id))) {

			// (request_id == null) :: The request parameter was not present in the query string.
			// e.g. http://hostname.com?a=b
			// ("".equals(request_id)) The request parameter was present in the query string but has no value.
			// e.g. http://hostname.com?param=&a=b
			LOG.info("ERROR: erroneous request_id or request_mode. ");
			ServletsAuxMethodsClass.goToAuthenticationSignout(request, response, LOG);
		}
		else {
			authForm = new AuthForm();
			authForm.setId(request_id);
			InputStream in = SocialAuthServlet.class.getClassLoader()
					.getResourceAsStream("oauth_consumer.properties");
			SocialAuthConfig conf = SocialAuthConfig.getDefault();
			conf.load(in);
			authForm.setSocialAuthManager(new SocialAuthManager());
			authForm.getSocialAuthManager().setSocialAuthConfig(conf);

			session.setAttribute("authForm", authForm);
			LOG.info("Redirecting to open authentication service for "+request_id);
			// String returnToUrl = RequestUtils.absoluteURL(request, "/socialAuthenticationServlet").toString();
			String returnToUrl = appUrl + "/SocialAuthLoggedInServlet";  
			newUrl = authForm.getSocialAuthManager().getAuthenticationUrl(request_id, returnToUrl);
			
		    if ((newUrl == null) || ("".equals(newUrl))) {
		    	LOG.info("ERROR: newUrl is empty or null !!! ");
		    	ServletsAuxMethodsClass.goToAuthenticationSignout(request, response, LOG);
		    }
		    else {
		    	LOG.info("Redirect to: " + newUrl);
		    	response.sendRedirect( newUrl );
		    	// ActionForward fwd = new ActionForward("openAuthUrl", url, true);
		    	// return fwd;
		    }
		}
	}
	
	private void socialAuthenticationSignOut(HttpSession session, HttpServletResponse response) {
		
		AuthForm authForm = (AuthForm) session.getAttribute("authForm");
		if ((authForm != null) && 
				(authForm.getSocialAuthManager() != null)) {
			authForm.getSocialAuthManager().disconnectProvider(authForm.getId());
			// String urlWithSessionID = response.encodeRedirectURL(aDestinationPage.toString());
			// response.sendRedirect( urlWithSessionID );
			// return mapping.findForward("home");
			// url = RequestUtils.absoluteURL(request, "/").toString();
		}
		session.invalidate();
	}
	
	private void socialAuthenticationSigned(HttpSession session, HttpServletResponse response) {
		
	}

}

