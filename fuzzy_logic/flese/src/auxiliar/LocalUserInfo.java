package auxiliar;

import logs.LogsManager;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import authProviders.AuthProviderInterface;
import storeHouse.RequestStoreHouse;
import storeHouse.RequestStoreHouseException;
import storeHouse.SessionStoreHouse;

public class LocalUserInfo {

	final Log LOG = LogFactory.getLog(LocalUserInfo.class);
	private String localUserName = null;

	/**
	 * Tests if the client session has been authenticated, which is why it needs
	 * request and response.
	 * 
	 * @param request
	 *            is the HttpServletRequest
	 * @param response
	 *            is the HttpServletResponse
	 * @throws RequestStoreHouseException
	 * @throws Exception
	 *             if request is null, response is null, session is null or
	 *             localUserName can not be set.
	 */
	private LocalUserInfo(RequestStoreHouse requestStoreHouse) throws Exception {

		localUserName = null;
		
		boolean appIsInTestingMode = requestStoreHouse.getSession().appIsInTestingMode();
		AuthProviderInterface authProvider = requestStoreHouse.getSession().getAuthProvider(); 
		localUserName = authProvider.getLocalUserName(appIsInTestingMode);

		if (localUserName == null) {
			String msg = "Impossible to create object LocalUserInfo because Variable localUserName is null";
			LOG.info(msg);
			throw new Exception(msg);
		} else {
			LogsManager.logSignedUser(this.localUserName);
		}
	}

	public static LocalUserInfo getLocalUserInfo(RequestStoreHouse requestStoreHouse) {
		LocalUserInfo localUserInfo = null;
		SessionStoreHouse sessionStoreHouse = null;
		if (requestStoreHouse != null) {
			sessionStoreHouse = requestStoreHouse.getSession();
			if (sessionStoreHouse != null) {
				localUserInfo = sessionStoreHouse.getLocalUserInfo();
			}
		}

		if (localUserInfo == null) {
			try {
				localUserInfo = new LocalUserInfo(requestStoreHouse);
			} catch (Exception e) {
				// e.printStackTrace();
				localUserInfo = null;
			}
			requestStoreHouse.getSession().setLocalUserInfo(localUserInfo);
		}
		return localUserInfo;

	}

	public String getLocalUserName() {
		return this.localUserName;
	}

}

/*----*/
