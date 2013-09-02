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

import storeHouse.RequestStoreHouse;
import storeHouse.RequestStoreHouseException;
import auxiliar.NextStep;
import constants.KConstants;
import constants.KUrls;
import filesAndPaths.PathsUtils;

/**
 * Servlet implementation class
 */
// @WebServlet(KConstants.servletName)
@WebServlet("/Servlet")
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

	private void doGetAndDoPost(String doMethod, HttpServletRequest request, HttpServletResponse response) throws ServletException,
			IOException {

		String managerName = request.getParameter(KConstants.Request.managerParam);
		LOG.info("STARTS Servlet. doAction: " + doMethod + " manager: " + (managerName != null ? managerName : ""));

		NextStep nextStep;
		InterfaceManager managerObject = getManager(managerName);
		nextStep = processRequest(managerObject, doMethod, request, response);

		if (nextStep == null) {
			nextStep = new NextStep(KConstants.NextStep.forward_to, KUrls.Pages.Exception, "");
		}
		
		try {
			nextStep.takeAction(request, response);
		} catch (Exception e) {
			e.printStackTrace();
		}

		LOG.info("ENDS Servlet. doAction: " + doMethod + " manager: " + (managerName != null ? managerName : ""));
	}

	private NextStep processRequest(InterfaceManager managerObject, String doMethod, HttpServletRequest request,
			HttpServletResponse response) {
		NextStep nextStep = new NextStep(KConstants.NextStep.forward_to, KUrls.Pages.Exception, "");
		if (managerObject == null) {
			return nextStep;
		}
		nextStep = managerObject.getExceptionPage();

		// Create the session if the manager needs it.
		boolean createSessionIfNull = managerObject.createSessionIfNull();
		ServletContext servletContext = getServletConfig().getServletContext();
		RequestStoreHouse sessionStoreHouse;
		try {
			sessionStoreHouse = new RequestStoreHouse(request, createSessionIfNull);
			sessionStoreHouse.setResponse(response);
			sessionStoreHouse.setServletContext(servletContext);
			sessionStoreHouse.setDoMethod(doMethod);
		} catch (RequestStoreHouseException e) {
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
			String managerClassFullName = KConstants.Managers.managersPackage + "." + managerName + KConstants.Managers.managerSuffix;
			try {
				managerClass =  Class.forName(managerClassFullName);
			} catch (ClassNotFoundException e) {
				e.printStackTrace();
				managerClass = null;
			}
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
