package servlets;

import java.io.IOException;

import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import storeHouse.SessionStoreHouse;
import urls.UrlMap;
import urls.UrlsMaps;
import auxiliar.DispatchersClass;
import auxiliar.LocalUserInfo;
import auxiliar.NextStep;
import auxiliar.ServletsAuxMethodsClass;
import constants.KConstants;

/**
 * Servlet implementation class SearchServlet
 */
@WebServlet("/DispatcherServlet")
public class DispatcherServlet extends HttpServlet {
	private static final long serialVersionUID = 1L;
	final Log LOG = LogFactory.getLog(DispatcherServlet.class);

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

	private void doGetAndDoPost(String doAction, HttpServletRequest request, HttpServletResponse response) throws ServletException,
			IOException {
		LOG.info("--- " + doAction + " invocation ---");

		try {
			// Dispatch the query.
			dispatchQuery(doAction, request, response);

		} catch (Exception e) {
			ServletsAuxMethodsClass.actionOnException(KConstants.Pages.ExceptionAjaxPage, "", e, request, response, LOG);
		}
		LOG.info("--- " + doAction + " end ---");
	}

	private void dispatchQuery(String doAction, HttpServletRequest request, HttpServletResponse response) throws Exception {

		SessionStoreHouse sessionStoreHouse = null;
		NextStep nextStep = null;

		try {
			// Sessions management.
			sessionStoreHouse = new SessionStoreHouse(request, response, false);
		} catch (Exception e) {
			sessionStoreHouse = null;
			nextStep = new NextStep(NextStep.Constants.forward_to, KConstants.Pages.NullSessionAjaxPage, "");
			nextStep.takeAction(request, response);

		}

		if (sessionStoreHouse != null) {
			nextStep = dispatchQueryWithSession(doAction, sessionStoreHouse);
			nextStep.takeAction(request, response);
		}

	}

	private NextStep dispatchQueryWithSession(String doAction, SessionStoreHouse sessionStoreHouse) throws Exception {

		// Tests if we have logged in.
		LocalUserInfo localUserInfo = sessionStoreHouse.getLocalUserInfo();

		// Needed to work.
		ServletContext servletContext = getServletConfig().getServletContext();
		DispatchersClass dispatcherObject = new DispatchersClass(servletContext, doAction, sessionStoreHouse);

		NextStep nextStep = null;

		String request_op = sessionStoreHouse.getRequestOp();
		if ((request_op != null) && (!"".equals(request_op))) {

			String requestName = request_op + "Request";
			UrlMap urlMap = UrlsMaps.getUrlMap(requestName);
			String methodName = urlMap.getKeyString();

			try {
				nextStep = dispatcherObject.getClass().getMethod(methodName, (Class<?>) null).invoke(null, null);
			} catch (NoSuchMethodException e) {
				e.printStackTrace();
				System.out.println(e.toString());
				request_op = null;
			}
		}

		if (request_op == null) {
			nextStep = dispatcherObject.emptyRequest();
		}
	}
}
