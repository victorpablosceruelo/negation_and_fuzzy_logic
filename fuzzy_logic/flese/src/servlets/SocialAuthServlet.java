
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


	
}




//// EOF


