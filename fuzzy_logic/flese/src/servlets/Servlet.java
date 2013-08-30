package servlets;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;

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

import storeHouse.SessionStoreHouse;
import storeHouse.SessionStoreHouseException;
import auxiliar.NextStep;
import constants.KConstants;
import constants.KUrls;

/**
 * Servlet implementation class
 */
@WebServlet(KConstants.servletName)
public class Servlet extends HttpServlet {
	private static final long serialVersionUID = 1L;
	final Log LOG = LogFactory.getLog(Servlet.class);

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

		String managerName = request.getParameter(KConstants.Request.managerParam);
		LOG.info("STARTS Servlet. doAction: " + doAction + " manager: " + (managerName != null ? managerName : ""));

		NextStep nextStep;
		InterfaceManager managerObject = getManager(managerName);
		nextStep = processRequest(managerObject, doAction, request, response);

		if (nextStep == null) {
			nextStep = new NextStep(NextStep.Constants.forward_to, KUrls.Pages.Exception, "");
			;
		}
		
		try {
			nextStep.takeAction(request, response);
		} catch (Exception e) {
			e.printStackTrace();
		}

		LOG.info("ENDS Servlet. doAction: " + doAction + " manager: " + (managerName != null ? managerName : ""));
	}

	private NextStep processRequest(InterfaceManager managerObject, String doAction, HttpServletRequest request,
			HttpServletResponse response) {
		NextStep nextStep = new NextStep(NextStep.Constants.forward_to, KUrls.Pages.Exception, "");
		if (managerObject == null) {
			return nextStep;
		}
		nextStep = managerObject.getExceptionPage();

		// Create the session if the manager needs it.
		boolean createSessionIfNull = managerObject.createSessionIfNull();
		ServletContext servletContext = getServletConfig().getServletContext();
		SessionStoreHouse sessionStoreHouse;
		try {
			sessionStoreHouse = new SessionStoreHouse(request, response, createSessionIfNull, servletContext, doAction);
		} catch (SessionStoreHouseException e) {
			e.printStackTrace();
			return nextStep;
		}

		// By-pass parameters to the manager.
		managerObject.setSessionStoreHouse(sessionStoreHouse);

		// Dispatch the query.
		try {
			nextStep = managerObject.processRequest();
		} catch (Exception e) {
			e.printStackTrace();
			nextStep = null;
		}
		if (nextStep == null)
			nextStep = managerObject.getExceptionPage();
		return nextStep;
	}

	@SuppressWarnings("unchecked")
	private InterfaceManager getManager(String managerName) {
		@SuppressWarnings("rawtypes")
		Class managerClass = null;
		InterfaceManager managerObject = null;

		if (managerName != null) {
			managerClass = (managerName + KConstants.Managers.managerSuffix).getClass();
		}

		if (managerClass == null) {
			managerClass = AuthManager.class;
		}

		try {
			managerObject = (InterfaceManager) (managerClass.getConstructor(new Class[0])).newInstance(new Object[0]);
		} catch (InstantiationException e) {
			managerObject = null;
			e.printStackTrace();
		} catch (IllegalAccessException e) {
			managerObject = null;
			e.printStackTrace();
		} catch (IllegalArgumentException e) {
			managerObject = null;
			e.printStackTrace();
		} catch (InvocationTargetException e) {
			managerObject = null;
			e.printStackTrace();
		} catch (NoSuchMethodException e) {
			managerObject = null;
			e.printStackTrace();
		} catch (SecurityException e) {
			managerObject = null;
			e.printStackTrace();
		}
		return managerObject;
	}
}

/* --- */
