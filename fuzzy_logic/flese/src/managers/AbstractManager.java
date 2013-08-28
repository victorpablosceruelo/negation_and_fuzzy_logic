package managers;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import servlets.Servlet.Constants;
import storeHouse.SessionStoreHouse;
import urls.UrlMap;
import urls.UrlsMaps;
import auxiliar.DispatchersClass;
import auxiliar.NextStep;
import constants.KUrls;

public abstract class AbstractManager implements InterfaceManager {

	String doAction = null; 
	HttpServletRequest request = null; 
	HttpServletResponse response = null;
	ServletContext servletContext = null;
	
	public AbstractManager(String doAction, HttpServletRequest request, HttpServletResponse response, ServletContext servletContext) {
		this.doAction = doAction;
		this.request = request;
		this.response = response;
		this.servletContext = servletContext;
	}
	
	private void dispatchQuery() throws Exception {

		SessionStoreHouse sessionStoreHouse = null;
		

		try {
			// Sessions management.
			
			sessionStoreHouse = new SessionStoreHouse(request, response, false, servletContext, doAction);
		} catch (Exception e) {
			sessionStoreHouse = null;
			nextStep = new NextStep(NextStep.Constants.forward_to, KUrls.NullSessionAjaxPage, "");
			

		}

		if (sessionStoreHouse != null) {
			nextStep = dispatchQueryWithSession(sessionStoreHouse);
			nextStep.takeAction(request, response);
		}

	}

	private NextStep dispatchQueryWithSession(SessionStoreHouse sessionStoreHouse) throws Exception {

		// Needed to work.

		DispatchersClass dispatcherObject = new DispatchersClass(sessionStoreHouse);

		NextStep nextStep = null;

		String request_op = sessionStoreHouse.getRequestParameter(Constants.Operation);
		if ((request_op != null) && (!"".equals(request_op))) {

			String requestName = request_op + "Request";
			UrlMap urlMap = UrlsMaps.getUrlMap(requestName);
			String methodName = urlMap.getKeyString();

			try {
				nextStep = (NextStep) dispatcherObject.getClass().getMethod(methodName, (Class<?>) null)
						.invoke((Object) null, (Object) null);
				return nextStep;
			} catch (NoSuchMethodException e) {
				e.printStackTrace();
				System.out.println(e.toString());
				request_op = null;
			}
		}

		nextStep = dispatcherObject.emptyRequest();
		return nextStep;
	}
}
