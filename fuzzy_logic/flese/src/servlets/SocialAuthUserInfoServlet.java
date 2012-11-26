
package servlets;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

// import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import auxiliar.LocalUserNameClass;
import auxiliar.ServletsAuxMethodsClass;

@WebServlet("/SocialAuthUserInfoServlet")
public class SocialAuthUserInfoServlet extends HttpServlet {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private static final Log LOG = LogFactory.getLog(SocialAuthUserInfoServlet.class);

	public void doGet(HttpServletRequest request, HttpServletResponse response) {
		doGetAndDoPost("doGet", request, response);
	}
	
	public void doPost(HttpServletRequest request, HttpServletResponse response) {
		doGetAndDoPost("doPost", request, response);
	}
	
	private void doGetAndDoPost(String doAction, HttpServletRequest request, HttpServletResponse response) {
		LOG.info("--- "+doAction+" invocation ---");
		try {
			@SuppressWarnings("unused")
			LocalUserNameClass localUserName = new LocalUserNameClass(request, response);
			ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.SocialAuthUserInfoPage, "", request, response, LOG);
		} catch (Exception e) {
			ServletsAuxMethodsClass.actionOnException(ServletsAuxMethodsClass.SocialAuthServletSignOut, "", e, request, response, LOG);
		}
		LOG.info("--- "+doAction+" end ---");
	}	
}




//// EOF


