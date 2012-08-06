package socialAuth;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import socialAuth.util.SocialAuthUtil;

/**
 * Servlet implementation class SocialAuthLoggedIn
 */
@WebServlet("/SocialAuthLoggedInServlet")
public class SocialAuthLoggedInServlet extends HttpServlet {
	private static final long serialVersionUID = 1L;
	private final Log LOG = LogFactory.getLog(SocialAuthLoggedInServlet.class);
       
    /**
     * @see HttpServlet#HttpServlet()
     */
    public SocialAuthLoggedInServlet() {
        super();
    }

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) 
			throws ServletException, IOException {
		LOG.info("doGet invocation. Calling doPost");
		doPost(request, response);
	}

	/**
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		try {
			socialAuthLoggedIn(request, response);
		} catch (Exception e) {
			System.out.print("Exception thrown: ");
			System.out.print(e);
			e.printStackTrace();
			
		}
	}

	protected void socialAuthLoggedIn(HttpServletRequest request, HttpServletResponse response) 
			throws ServletException, IOException {
		
		HttpSession session = request.getSession(false);
		if (session == null) {
			LOG.info("session is null");
			AuxMethodsClass.redirectToAuthenticationIndex(request, response);
			return;
		}
		
		// String appUrl = AuxMethodsClass.getAppUrlFromRequest(request);
		AuthForm authForm = (AuthForm) session.getAttribute("authForm");;
		if ((authForm != null) && (authForm.getSocialAuthManager() != null)) {
			List<Contact> contactsList = new ArrayList<Contact>();
			Profile profile = null;
			try {
				Map<String, String> paramsMap = SocialAuthUtil.getRequestParametersMap(request);
				AuthProvider provider = authForm.getSocialAuthManager().connect(paramsMap);

				profile = provider.getUserProfile();
				contactsList = provider.getContactList();
				if (contactsList != null && contactsList.size() > 0) {
					for (Contact p : contactsList) {
						if (StringUtils.isEmpty(p.getFirstName()) && StringUtils.isEmpty(p.getLastName())) {
							p.setFirstName(p.getDisplayName());
						}
					}
				}
			} catch (Exception e) {
				e.printStackTrace();
			}
			request.setAttribute("profile", profile);
			request.setAttribute("contacts", contactsList);

			// return mapping.findForward("success");
			AuxMethodsClass.forward_to("SocialAuthUpdateStatusServlet", request, response);
		}
		else {
			if (authForm == null) {
				LOG.info("authForm is null");
				request.setAttribute("msg", "authForm is null");
			}
			else {
				LOG.info("authForm.getSocialAuthManager is null");
				request.setAttribute("msg", "authForm.getSocialAuthManager is null");
			}
				
		}
		// if provider null
		// return mapping.findForward("failure");
		AuxMethodsClass.forward_to("error.jsp", request, response);
	}
	
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
}
