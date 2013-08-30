package managers;

import storeHouse.SessionStoreHouse;
import urls.UrlMap;
import urls.UrlsMaps;
import auxiliar.NextStep;
import constants.KConstants;

public abstract class AbstractManager implements InterfaceManager {

	protected SessionStoreHouse sessionStoreHouse;

	public void setSessionStoreHouse(SessionStoreHouse sessionStoreHouse) {
		this.sessionStoreHouse = sessionStoreHouse;
	}

	protected String getRequestManager() {
		return sessionStoreHouse.getRequestParameter(KConstants.Request.managerParam);
	}

	protected String getRequestOp() {
		return sessionStoreHouse.getRequestParameter(KConstants.Request.operationParam);
	}

	public NextStep processRequest() throws Exception {

		NextStep nextStep = null;

		UrlMap urlMap = new UrlMap(sessionStoreHouse);
		urlMap = UrlsMaps.getUrlMap(urlMap);

		String op = urlMap.getOp();
		if ((op != null) && (!"".equals(op))) {
			nextStep = (NextStep) this.getClass().getMethod(op, (Class<?>) null).invoke((Object) null, (Object) null);
		} else {
			nextStep = byDefaultMethod();
		}
		return nextStep;
	}
}
