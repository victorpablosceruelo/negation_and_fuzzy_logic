package managers;

import storeHouse.RequestStoreHouse;
import urls.UrlMap;
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

	public NextStep processRequest() throws Exception {

		NextStep nextStep = null;

		UrlMap urlMap = new UrlMap(requestStoreHouse);
		urlMap = UrlsMaps.getUrlMap(urlMap);

		String op = urlMap.getOp();
		if ((op != null) && (!"".equals(op))) {
			try {
			nextStep = (NextStep) this.getClass().getMethod(op, (Class<?>) null).invoke((Object) null, (Object) null);
			} catch (IllegalAccessException e) {
				e.printStackTrace();
				op = null;
			}
		} 
		// This allows a failsafe when the method is not found or there is no method.
		if ((op == null) || ("".equals(op))) {
			nextStep = byDefaultMethod();
		}
		return nextStep;
	}
}
