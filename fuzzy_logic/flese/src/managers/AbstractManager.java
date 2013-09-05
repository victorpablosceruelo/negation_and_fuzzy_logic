package managers;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import storeHouse.RequestStoreHouse;
import urls.UrlMap;
import urls.UrlMapException;
import urls.UrlsMaps;
import auxiliar.NextStep;
import constants.KConstants;
import constants.KUrls;

public abstract class AbstractManager implements InterfaceManager {

	private static final Log LogAbstractManager = LogFactory.getLog(AbstractManager.class);
	
	protected RequestStoreHouse requestStoreHouse;
	private NextStep nextStep;
	
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

		UrlMap urlMap = new UrlMap(requestStoreHouse);
		try {
			urlMap = UrlsMaps.getUrlMap(urlMap);
		} catch (UrlMapException e1) {
			e1.printStackTrace();
			urlMap = null;
		}

		if (urlMap != null) {
			op = urlMap.getOp();
			if ((op != null) && (!"".equals(op))) {

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
				if (method != null) {
					try {
						LogAbstractManager.info("Calling method " + method.getName() + " in class " + this.getClass().getName());
						method.invoke((Object) this, new Object [0]);
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
						setNextStep(new NextStep(KConstants.NextStep.forward_to, KUrls.Pages.Exception, ""));
						e.printStackTrace();
					} 
				}

			}
		}
		// This allows a failsafe when the method is not found or there is no
		// method.
		if ((urlMap == null) || (method == null) || (op == null) || ("".equals(op))) {
			try {
				LogAbstractManager.info("Invoke byDefaultMethod ");
				byDefaultMethod();
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
		
		return nextStep;
	}
}
