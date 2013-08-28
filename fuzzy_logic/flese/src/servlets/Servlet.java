package servlets;

import java.io.IOException;

import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import managers.AuthManager;
import managers.InterfaceManager;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import auxiliar.NextStep;
import constants.KUrls;

/**
 * Servlet implementation class
 */
@WebServlet("/Servlet")
public class Servlet extends HttpServlet {
	private static final long serialVersionUID = 1L;
	final Log LOG = LogFactory.getLog(Servlet.class);

	private static final class KConstants {
		public static final String managerParam = "manager";
		public static final String managerSuffix = "Manager";
	}

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse
	 *      response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		doGetAndDoPost("doGet", request, response);
	}

	/**
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse
	 *      response)
	 */
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		doGetAndDoPost("doPost", request, response);
	}

	@SuppressWarnings("unchecked")
	private void doGetAndDoPost(String doAction, HttpServletRequest request, HttpServletResponse response) throws ServletException,
			IOException {

		String managerName = request.getParameter(KConstants.managerParam);
		LOG.info("STARTS Servlet. doAction: " + doAction + " manager: " + (managerName != null ? managerName : ""));

		ServletContext servletContext = getServletConfig().getServletContext();

		@SuppressWarnings("rawtypes")
		Class managerClass = null;
		@SuppressWarnings("rawtypes")
		Class[] parametersTypes = { String.class, HttpServletRequest.class, HttpServletResponse.class, ServletContext.class };
		Object[] parametersValues = { doAction, request, response, servletContext };
		InterfaceManager managerObject = null;
		NextStep nextStep = null;

		try {
			if (managerName != null) {
				managerClass = (managerName + KConstants.managerSuffix).getClass();
			}

			if (managerClass == null) {
				managerClass = AuthManager.class;
			}

			managerObject = (InterfaceManager) (managerClass.getConstructor(parametersTypes)).newInstance(parametersValues);
			// Dispatch the query.
			managerObject.processRequest();

		} catch (Exception e) {
			if (managerObject != null) {
				nextStep = managerObject.getExceptionPage();
			} else {
				try {
					nextStep = new NextStep(NextStep.Constants.forward_to, KUrls.Pages.ExceptionAjaxPage, "");
				} catch (Exception e1) {
					e1.printStackTrace();
				}
			}

		}

		if (nextStep != null) {
			try {
				nextStep.takeAction(request, response);
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
		LOG.info("ENDS Servlet. doAction: " + doAction + " manager: " + (managerName != null ? managerName : ""));
	}

}

/* --- */
