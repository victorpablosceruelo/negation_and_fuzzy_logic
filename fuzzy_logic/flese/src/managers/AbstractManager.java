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
	private Method method;
	private String op;

	public void actionWhenExceptionInTargetMethodInvocation(String methodName) {
		NextStep nextStep = new NextStep(KConstants.NextStep.forward_to, KUrls.Pages.Exception, "");
		setNextStep(nextStep);
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

	public void setRequestStoreHouse(RequestStoreHouse requestStoreHouse) {
		this.requestStoreHouse = requestStoreHouse;
	}

	protected String getRequestManager() {
		return requestStoreHouse.getRequestParameter(KConstants.Request.managerParam);
	}

	protected String getRequestOp() {
		return requestStoreHouse.getRequestParameter(KConstants.Request.operationParam);
	}

	public NextStep processRequest() {

		op = null;
		method = null;
		setNextStep(null);
		UrlMap urlMap = getValidUrlMap();

		// Get the results storage facility.
		this.resultsStoreHouse = this.requestStoreHouse.getResultsStoreHouse(true);

		if (urlMap != null) {
			op = urlMap.getOp(false);
			if ((op != null) && (!"".equals(op))) {
				getMethod();
				invokeMethod();
			}
		}
		// This allows a failsafe when the method is not found or there is no
		// method.
		if ((urlMap == null) || (method == null) || (op == null) || ("".equals(op))) {
			invokeDefaultMethod();
		}

		// Save results in the request, to access them from jsps.
		this.requestStoreHouse.storeResultsStoreHouse();

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

	private void getMethod() {
		try {
			method = this.getClass().getMethod(op, new Class[] {});
		} catch (NoSuchMethodException e) {
			method = null;
			LogAbstractManager.error("ERROR looking for method " + op + " in class " + this.getClass().getName());
			setNextStep(null);
			e.printStackTrace();
		} catch (SecurityException e) {
			method = null;
			LogAbstractManager.error("ERROR looking for method " + op + " in class " + this.getClass().getName());
			setNextStep(null);
			e.printStackTrace();
		}
	}

	private void invokeMethod() {
		if (method != null) {
			try {
				LogAbstractManager.info("Calling method " + method.getName() + " in class " + this.getClass().getName());
				method.invoke((Object) this, new Object[0]);
			} catch (IllegalAccessException e) {
				LogAbstractManager.error("ERROR invoking method " + method.getName() + " in class " + this.getClass().getName());
				e.printStackTrace();
				setNextStep(null);
			} catch (IllegalArgumentException e) {
				LogAbstractManager.error("ERROR invoking method " + method.getName() + " in class " + this.getClass().getName());
				e.printStackTrace();
				setNextStep(null);
			} catch (InvocationTargetException e) {
				actionWhenExceptionInTargetMethodInvocationEnvelope(e, method.getName(), this.getClass().getName());
			}
		}
	}

	private void invokeDefaultMethod() {
		try {
			LogAbstractManager.info("Invoke byDefaultMethod ");
			byDefaultMethod();
		} catch (Exception e) {
			actionWhenExceptionInTargetMethodInvocationEnvelope(e, "byDefaultMethod", this.getClass().getName());
		}
	}

	private void actionWhenExceptionInTargetMethodInvocationEnvelope(Exception e, String methodName, String className) {
		if (methodName == null)
			methodName = "unknown";
		if (className == null)
			className = "unknown";

		LogAbstractManager.error("Exception executing method " + methodName + " in class " + className);
		if ((e.getMessage() != null) && (!"".equals(e.getMessage()))) {
			LogAbstractManager.error(e.getMessage());
		}
		LogAbstractManager.error("------------------------------");
		e.printStackTrace();
		LogAbstractManager.error("------------------------------");

		saveExceptionInformation(e, className, methodName);
		actionWhenExceptionInTargetMethodInvocation(methodName);
	}

	private void saveExceptionInformation(Exception e, String className, String methodName) {
		if (className == null) {
			className = "unknown";
		}
		if (methodName == null) {
			methodName = "unknown";
		}

		StringBuilder msg = new StringBuilder();
		msg.append("Exception in method ");
		msg.append(methodName);
		msg.append(" of class ");
		msg.append(className);

		if ((e != null) && (e.getMessage() != null) && (!"".equals(e.getMessage()))) {
			msg.append(": ");
			msg.append(e.getMessage());
		}
		resultsStoreHouse.setExceptionMsg(msg.toString());
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

}



// EOF
