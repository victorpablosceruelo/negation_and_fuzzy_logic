package managers;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import results.ResultsStoreHouse;
import storeHouse.RequestStoreHouse;
import urls.UrlMap;
import urls.UrlMapException;
import urls.UrlsMaps;
import auxiliar.NextStep;
import constants.KConstants;
import constants.KUrls;

public abstract class AbstractManager implements InterfaceManager {

	private static final Log LogAbstractManager = LogFactory.getLog(AbstractManager.class);

	protected RequestStoreHouse requestStoreHouse = null;
	protected ResultsStoreHouse resultsStoreHouse = null;
	private NextStep nextStep;

	public NextStep getExceptionPage() {
		NextStep nextStep = new NextStep(KConstants.NextStep.forward_to, KUrls.Pages.Exception, "");
		return nextStep;
	}
	
	public boolean createSessionIfNull() {
		return false;
	}
	
	public boolean exceptionIfSessionIsNull() {
		return true;
	}
	
	public boolean exceptionIfLocalUserInfoIsNull() {
		return true;
	}
	
	public boolean reinitializeResultsStoreHouse() {
		return true;
	}

	public NextStep getNextStep() {
		return nextStep;
	}

	public void setNextStep(NextStep nextStep) {
		this.nextStep = nextStep;
	}

	public void setSessionStoreHouse(RequestStoreHouse sessionStoreHouse) {
		this.requestStoreHouse = sessionStoreHouse;
	}

	protected String getRequestManager() {
		return requestStoreHouse.getRequestParameter(KConstants.Request.managerParam);
	}

	protected String getRequestOp() {
		return requestStoreHouse.getRequestParameter(KConstants.Request.operationParam);
	}

	public NextStep processRequest() {

		String op = null;
		Method method = null;
		setNextStep(null);
		UrlMap urlMap = getValidUrlMap();

		// Get the results storage facility.
		this.getResultsStoreHouse();

		if (urlMap != null) {
			op = urlMap.getOp(false);
			if ((op != null) && (!"".equals(op))) {
				method = getMethod(op);
				invokeMethod(method);
			}
		}
		// This allows a failsafe when the method is not found or there is no
		// method.
		if ((urlMap == null) || (method == null) || (op == null) || ("".equals(op))) {
			invokeDefaultMethod();
		}

		// Save results in the request, to access them from jsps.
		setResultsStoreHouse();
		
		return nextStep;
	}

	private UrlMap getValidUrlMap() {
		UrlMap urlMap = new UrlMap(requestStoreHouse);
		try {
			urlMap = UrlsMaps.getUrlMap(urlMap);
		} catch (UrlMapException e1) {
			e1.printStackTrace();
			urlMap = null;
		}
		return urlMap;
	}

	private Method getMethod(String op) {
		Method method = null;
		try {
			method = this.getClass().getMethod(op, new Class[] {});
		} catch (NoSuchMethodException e) {
			LogAbstractManager.error("ERROR looking for method " + op + " in class " + this.getClass().getName());
			setNextStep(null);
			e.printStackTrace();
		} catch (SecurityException e) {
			LogAbstractManager.error("ERROR looking for method " + op + " in class " + this.getClass().getName());
			setNextStep(null);
			e.printStackTrace();
		}
		return method;
	}

	private void invokeMethod(Method method) {
		if (method != null) {
			try {
				LogAbstractManager.info("Calling method " + method.getName() + " in class " + this.getClass().getName());
				method.invoke((Object) this, new Object[0]);
			} catch (IllegalAccessException e) {
				LogAbstractManager.error("ERROR invoking method " + method.getName() + " in class " + this.getClass().getName());
				setNextStep(null);
				e.printStackTrace();
			} catch (IllegalArgumentException e) {
				LogAbstractManager.error("ERROR invoking method " + method.getName() + " in class " + this.getClass().getName());
				setNextStep(null);
				e.printStackTrace();
			} catch (InvocationTargetException e) {
				LogAbstractManager.error("Exception executing method " + method.getName() + " in class " + this.getClass().getName());
				NextStep onExceptionNextStep = getExceptionPage();
				setNextStep(onExceptionNextStep);
				e.printStackTrace();
				resultsStoreHouse.addExceptionMessage(e.getMessage());
			}
		}
	}

	private void invokeDefaultMethod() {
		try {
			LogAbstractManager.info("Invoke byDefaultMethod ");
			byDefaultMethod();
		} catch (Exception e) {
			NextStep onExceptionNextStep = getExceptionPage();
			setNextStep(onExceptionNextStep);
			e.printStackTrace();
		}
	}

	public void getResultsStoreHouse() {
		if (reinitializeResultsStoreHouse()) {
			this.resultsStoreHouse = null;
			setResultsStoreHouse();
		}

		this.resultsStoreHouse = (ResultsStoreHouse) this.requestStoreHouse.getRequest().getAttribute(KConstants.Request.resultsStoreHouse);
		
		if (this.resultsStoreHouse == null) {
			this.resultsStoreHouse = new ResultsStoreHouse();
		}
	}

	public void setResultsStoreHouse() {
		this.requestStoreHouse.getRequest().removeAttribute(KConstants.Request.resultsStoreHouse);
		if (resultsStoreHouse != null) {
			this.requestStoreHouse.getRequest().setAttribute(KConstants.Request.resultsStoreHouse, this.resultsStoreHouse);
		}
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

}
