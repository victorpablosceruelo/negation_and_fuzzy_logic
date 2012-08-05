/*
 ===========================================================================
 Copyright (c) 2010 BrickRed Technologies Limited

 Permission is hereby granted, free of charge, to any person obtaining a copy
 of this software and associated documentation files (the "Software"), to deal
 in the Software without restriction, including without limitation the rights
 to use, copy, modify, merge, publish, distribute, sub-license, and/or sell
 copies of the Software, and to permit persons to whom the Software is
 furnished to do so, subject to the following conditions:

 The above copyright notice and this permission notice shall be included in
 all copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 THE SOFTWARE.
 ===========================================================================

 */
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

import auxiliar.ServletsAuxClass;

import socialAuth.SocialAuthConfig;
import socialAuth.SocialAuthManager;
import socialAuth.AuthForm;
// import socialAuth.exception.SocialAuthException;
// import socialAuth.util.SocialAuthUtil;

// Do not use struts !!!
// import org.apache.struts.action.Action;
// import org.apache.struts.action.ActionForm;
// import org.apache.struts.action.ActionForward;
// import org.apache.struts.action.ActionMapping;
// import org.apache.struts.util.RequestUtils;


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

@WebServlet("/SocialAuthenticationServlet")
public class SocialAuthenticationServlet extends HttpServlet {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	final Log LOG = LogFactory.getLog(SocialAuthenticationServlet.class);

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
			login_or_logout(request, response);
		} catch (Exception e) {
			System.out.print("Exception thrown: ");
			System.out.print(e);
			e.printStackTrace();
			
		}
	}
		
	private void login_or_logout(HttpServletRequest request, HttpServletResponse response)
			throws Exception {
		
		HttpSession session = request.getSession(true);
		String url = "";
		LOG.info("login_or_logout entered");
		
		// Get the value of the parameter; the name is case-sensitive
		String request_id = request.getParameter("id");
		String request_mode = request.getParameter("mode");
	    if ((request_id == null) || ("".equals(request_id)) ||
	    	(request_mode == null) || ("".equals(request_mode))	) { 
	        // (request_id == null) :: The request parameter was not present in the query string.
	        // e.g. http://hostname.com?a=b
	        // ("".equals(request_id)) The request parameter was present in the query string but has no value.
	        // e.g. http://hostname.com?param=&a=b
	    	ServletsAuxClass.show_request_parameters(request, LOG);
	    	url = ServletsAuxClass.getAppUrlFromRequest(request, LOG);
	    	url += "/index-authentication.jsp";
			response.sendRedirect( url );
			return;
	    }
	    
		
		// AuthForm authForm = (AuthForm) form;
		AuthForm authForm = (AuthForm) session.getAttribute("authForm");
		authForm.setId(request_id);
		SocialAuthManager manager = null;
		
		if ("signin".equals(request_mode)) {
			InputStream in = SocialAuthenticationServlet.class.getClassLoader()
					.getResourceAsStream("oauth_consumer.properties");
			SocialAuthConfig conf = SocialAuthConfig.getDefault();
			conf.load(in);
			manager = new SocialAuthManager();
			manager.setSocialAuthConfig(conf);
			authForm.setSocialAuthManager(manager);
			
			LOG.info("Redirecting to open authentication service for "+request_id);
			// String returnToUrl = RequestUtils.absoluteURL(request, "/socialAuthenticationServlet").toString();
			String returnToUrl = ServletsAuxClass.getAppUrlFromRequest(request, LOG); 
			returnToUrl += "/socialAuthenticationServlet"; 
			url = manager.getAuthenticationUrl(request_id, returnToUrl);
			LOG.info("Redirecting to: " + url);
			if (url != null) {
				// ActionForward fwd = new ActionForward("openAuthUrl", url, true);
				// return fwd;
				response.sendRedirect( url );
				return;
			}

		}
		
		if ("signout".equals(request_mode)) {
			if (authForm.getSocialAuthManager() != null) {
				manager = authForm.getSocialAuthManager();
				manager.disconnectProvider(authForm.getId());
			    // String urlWithSessionID = response.encodeRedirectURL(aDestinationPage.toString());
			    // response.sendRedirect( urlWithSessionID );
				// return mapping.findForward("home");
				// url = RequestUtils.absoluteURL(request, "/").toString();
				url = ServletsAuxClass.getAppUrlFromRequest(request, LOG);
				response.sendRedirect( url );
				return;
			}
		}
		
		System.out.println("ERROR: Last line in doPost");
		// return mapping.findForward("failure");
		
	}
	
	/**
	 * Displays the user profile and contacts for the given provider.
	 * 
	 * @param mapping
	 *            the action mapping
	 * @param form
	 *            the action form
	 * @param request
	 *            the http servlet request
	 * @param response
	 *            the http servlet response
	 * @return ActionForward where the action should flow
	 * @throws IOException 
	 * @throws Exception
	 *             if an error occurs
	 */

	
/*	private ActionForward execute(final ActionMapping mapping,
			final ActionForm form, final HttpServletRequest request,
			final HttpServletResponse response) throws Exception {

		AuthForm authForm = (AuthForm) form;


		String id = authForm.getId();
		SocialAuthManager manager;
		if (authForm.getSocialAuthManager() != null) {
			manager = authForm.getSocialAuthManager();
			if ("signout".equals(request.getParameter("mode"))) {
				manager.disconnectProvider(id);
				return mapping.findForward("home");
			}
		} else {
			InputStream in = SocialAuthenticationAction.class.getClassLoader()
					.getResourceAsStream("oauth_consumer.properties");
			SocialAuthConfig conf = SocialAuthConfig.getDefault();
			conf.load(in);
			manager = new SocialAuthManager();
			manager.setSocialAuthConfig(conf);
			authForm.setSocialAuthManager(manager);
		}

		String returnToUrl = RequestUtils.absoluteURL(request,
				"/socialAuthSuccessAction.do").toString();
		String url = manager.getAuthenticationUrl(id, returnToUrl);
		LOG.info("Redirecting to: " + url);
		if (url != null) {
			ActionForward fwd = new ActionForward("openAuthUrl", url, true);
			return fwd;
		}
		return mapping.findForward("failure");
	}
*/
	
	/*	@Override
	public ActionForward execute(final ActionMapping mapping,
			final ActionForm form, final HttpServletRequest request,
			final HttpServletResponse response) throws Exception {

		AuthForm authForm = (AuthForm) form;
		SocialAuthManager manager = null;
		if (authForm.getSocialAuthManager() != null) {
			manager = authForm.getSocialAuthManager();
		}
		if (manager != null) {
			List<Contact> contactsList = new ArrayList<Contact>();
			Profile profile = null;
			try {
				Map<String, String> paramsMap = SocialAuthUtil
						.getRequestParametersMap(request);
				AuthProvider provider = manager.connect(paramsMap);

				profile = provider.getUserProfile();
				contactsList = provider.getContactList();
				if (contactsList != null && contactsList.size() > 0) {
					for (Contact p : contactsList) {
						if (StringUtils.isEmpty(p.getFirstName())
								&& StringUtils.isEmpty(p.getLastName())) {
							p.setFirstName(p.getDisplayName());
						}
					}
				}
			} catch (Exception e) {
				e.printStackTrace();
			}
			request.setAttribute("profile", profile);
			request.setAttribute("contacts", contactsList);

			return mapping.findForward("success");
		}
		// if provider null
		return mapping.findForward("failure");
	}
*/

/*
	@Override
	public ActionForward execute(final ActionMapping mapping,
			final ActionForm form, final HttpServletRequest request,
			final HttpServletResponse response) throws Exception {
		String statusMsg = request.getParameter("statusMessage");
		if (statusMsg == null || statusMsg.trim().length() == 0) {
			request.setAttribute("Message", "Status can't be left blank.");
			return mapping.findForward("failure");
		}
		AuthForm authForm = (AuthForm) form;
		SocialAuthManager manager = authForm.getSocialAuthManager();
		AuthProvider provider = null;
		if (manager != null) {
			provider = manager.getCurrentAuthProvider();
		}
		if (provider != null) {
			try {
				provider.updateStatus(statusMsg);
				request.setAttribute("Message", "Status Updated successfully");
				return mapping.findForward("success");
			} catch (SocialAuthException e) {
				request.setAttribute("Message", e.getMessage());
				e.printStackTrace();
			}
		}
		// if provider null
		return mapping.findForward("failure");
	}
*/


	

}

