package socialAuth;

// import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import socialAuth.exception.SocialAuthException;


@WebServlet("/SocialAuthUpdateStatusServlet")
public class SocialAuthUpdateStatusServlet extends HttpServlet {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private final Log LOG = LogFactory.getLog(SocialAuthUpdateStatusServlet.class);

	public void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, java.io.IOException {
			LOG.info("doGet invocation. Calling doPost");
			doPost(request, response);
	}
	
	public void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, java.io.IOException {
		
		
		try {
			statusUpdate(request, response);
		} catch (Exception e) {
			System.out.print("Exception thrown: ");
			System.out.print(e);
			e.printStackTrace();
			
		}
	}


	private void statusUpdate(HttpServletRequest request, HttpServletResponse response) throws Exception {
		
		HttpSession session = request.getSession(false);
		// String appUrl = AuxMethodsClass.getAppUrlFromRequest(request);
		// String newUrl = appUrl + "/index-authentication.jsp";

		if (session == null) {
			LOG.info("session is null");
			AuxMethodsClass.goToAppIndex(request, response, LOG);
			return;
		}
		
		/*
		 * ERROR AQUI: Este parametro no consigo q aparezca !!!!
		 */
		String statusMsg = request.getParameter("statusMessage");
		if ((statusMsg == null) || (statusMsg.trim().length() == 0)) {
			LOG.info("ERROR: statusMsg is null or empty.");
			if (statusMsg != null) {
				LOG.info("statusMsg: " + statusMsg);
			}
			AuxMethodsClass.goToError(request, response, LOG);
			return;
		}
		
		AuthForm authForm = (AuthForm) session.getAttribute("authForm");
		if ((authForm != null) && 
				(authForm.getSocialAuthManager() != null)) {
			
			AuthProvider provider = authForm.getSocialAuthManager().getCurrentAuthProvider();
			
			if (provider != null) {
				try {
					provider.updateStatus(statusMsg);
					request.setAttribute("msg1", "Status Updated successfully");
					AuxMethodsClass.goToSearchMenu(request, response, LOG);
					return;
				} catch (SocialAuthException e) {
					request.setAttribute("msg1", e.getMessage());
					e.printStackTrace();
				}
			}
			else {
				// if provider null
				LOG.info("provider is null");
				request.setAttribute("msg2", "provider is null");
			}

		}
		else {
			if (authForm == null) {
				LOG.info("authForm is null");
				request.setAttribute("msg2", "authForm is null");
			}
			else {
				LOG.info("authForm.getSocialAuthManager is null");
				request.setAttribute("msg2", "authForm.getSocialAuthManager is null");
			}
				
		}
		// if authForm null
		AuxMethodsClass.goToAuthenticationLogout(request, response, LOG);
		return;
	}
	
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
