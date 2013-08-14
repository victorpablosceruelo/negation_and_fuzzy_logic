package servlets;

import java.io.IOException;

import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import urls.UrlMap;
import urls.UrlsMaps;
import auxiliar.DispatchersClass;
import auxiliar.LocalUserNameClass;
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

		// Ask for an existing session.
		HttpSession session = request.getSession(false);
		if (session == null) {
			ServletsAuxMethodsClass.forward_to(KConstants.Pages.NullSessionAjaxPage, "", request, response, LOG);
		} else {
			dispatchQueryWithSession(doAction, request, response, session);
		}

	}

	private void dispatchQueryWithSession(String doAction, HttpServletRequest request, HttpServletResponse response, HttpSession session)
			throws Exception {

		if (session == null)
			throw new Exception("Session is null");

		// Tests if we have logged in.
		LocalUserNameClass localUserName = new LocalUserNameClass(request, response);

		// Needed to work.
		ServletContext servletContext = getServletConfig().getServletContext();
		DispatchersClass dispatcherObject = new DispatchersClass(servletContext, doAction, localUserName, request, response);

		String request_op = request.getParameter("op");

		if ((request_op != null) && (!"".equals(request_op))) {

			String requestName = request_op + "Request";
			UrlMap urlMap = UrlsMaps.getUrlMap(requestName);
			String methodName = urlMap.getKeyString();

			try {
				dispatcherObject.getClass().getMethod(methodName, (Class<?>) null);
			} catch (NoSuchMethodException e) {
				e.printStackTrace();
				System.out.println(e.toString());
				request_op = null;
			}
		}

		if (request_op == null) {
			dispatcherObject.emptyRequest();
		}
	}
}
