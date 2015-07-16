package auxiliar;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import authProviders.AuthProviderInterface;
import authProviders.Utils;
import logs.LogsManager;
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
		if (authProvider == null) {
			String msg = "Impossible to create object LocalUserInfo because authProvider is null";
			LOG.info(msg);
			throw new Exception(msg);
		}
		localUserName = authProvider.getLocalUserName(appIsInTestingMode);
		check();

		// If auth manager is not able to return a correct username, fix it.
		if (localUserName.contains("@")) {
			localUserName = Utils.ifNullThenSetUserNameFrom(null, localUserName, null, "", "");
			check();
		}

		// Ensure no spaces around.
		localUserName = localUserName.trim();
		check();

		LogsManager.logSignedUser(this.localUserName);
	}

	private LocalUserInfo(String localUserNameIn) throws Exception {
		// Ensure no spaces around.
		localUserName = localUserNameIn.trim();
		check();

		LogsManager.logSignedUser(this.localUserName);
	}

	private void check() throws Exception {
		if (StringsUtil.isEmptyString(localUserName)) {
			String msg = "Impossible to create object LocalUserInfo because Variable localUserName is empty or null.";
			LOG.info(msg);
			throw new Exception(msg);
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
			try {
				requestStoreHouse.getSession().setLocalUserInfo(localUserInfo);
			} catch (FleSeException e) {
				e.printStackTrace();
			}
		}
		return localUserInfo;
	}

	public static LocalUserInfo getFakeLocalUserInfo(String localUserName) throws Exception {
		return new LocalUserInfo(localUserName);
	}

	public String getLocalUserName() {
		return this.localUserName;
	}

}

/*----*/
