package managers;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import storeHouse.RequestStoreHouse;
import urls.UrlMap;
import urls.UrlMapException;
import urls.UrlsMaps;
import auxiliar.NextStep;
import constants.KConstants;

public abstract class AbstractManager implements InterfaceManager {

	protected RequestStoreHouse requestStoreHouse;

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

		NextStep nextStep = null;
		String op = null;

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

				@SuppressWarnings("rawtypes")
				Class[] argTypes = new Class[] {};
				Method method = null;
				try {
					method = this.getClass().getMethod(op, argTypes);
				} catch (NoSuchMethodException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (SecurityException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				if (method != null) {
					try {
						nextStep = (NextStep) method.invoke((Object) this, new Object [0]);
					} catch (IllegalAccessException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (IllegalArgumentException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (InvocationTargetException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (Exception e) {
						e.printStackTrace();
						// throw e;
					}
				}

			}
		}
		// This allows a failsafe when the method is not found or there is no
		// method.
		if ((urlMap == null) || (op == null) || ("".equals(op))) {
			try {
				nextStep = byDefaultMethod();
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		return nextStep;
	}
}
