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


@WebServlet("/socialAuthUpdateStatusServlet")
public class SocialAuthStatusUpdateServlet extends HttpServlet {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	final Log LOG = LogFactory.getLog(SocialAuthStatusUpdateServlet.class);

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
			AuxMethodsClass.redirectToAuthenticationIndex(request, response);
			return;
		}
		
		String statusMsg = request.getParameter("statusMessage");
		if ((statusMsg == null) || (statusMsg.trim().length() == 0)) {
			LOG.info("ERROR: statusMsg is null or empty.");
			if (statusMsg != null) {
				LOG.info("statusMsg: " + statusMsg);
			}
			AuxMethodsClass.redirectToError(request, response);
			return;
		}
		
		AuthForm authForm = (AuthForm) session.getAttribute("authForm");
		if ((authForm != null) && 
				(authForm.getSocialAuthManager() != null)) {
			
			AuthProvider provider = null;
			if (authForm.getSocialAuthManager() != null) {
				provider = authForm.getSocialAuthManager().getCurrentAuthProvider();
			}
			
			if (provider != null) {
				try {
					provider.updateStatus(statusMsg);
					request.setAttribute("Message", "Status Updated successfully");
					AuxMethodsClass.redirectToSearch(request, response);
					return;
				} catch (SocialAuthException e) {
					request.setAttribute("Message", e.getMessage());
					e.printStackTrace();
				}
			}
			// if provider null
			AuxMethodsClass.redirectToError(request, response);
			return;

		}
		// if authForm null
		AuxMethodsClass.redirectToLogout(request, response);
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
