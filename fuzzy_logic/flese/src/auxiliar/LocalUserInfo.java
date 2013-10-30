package auxiliar;

import logs.LogsManager;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.brickred.socialauth.Profile;

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

		Profile profile = requestStoreHouse.getSession().getUserProfile();
		if (profile == null) {
			if (requestStoreHouse.getSession().appIsInTestingMode()) {
				ifNullThenSetUserNameFrom("Testing User", "localhost.localnet", "testing", "testing");
			}
		} else {
			ifNullThenSetUserNameFrom(profile.getEmail(), profile.getProviderId(), "email", "providerId");
			ifNullThenSetUserNameFrom(profile.getDisplayName(), profile.getProviderId(), "displayName", "providerId");
			ifNullThenSetUserNameFrom(profile.getFullName(), profile.getProviderId(), "fullName", "providerId");
			ifNullThenSetUserNameFrom(profile.getFirstName(), profile.getProviderId(), "firstName", "providerId");
			ifNullThenSetUserNameFrom(profile.getLastName(), profile.getProviderId(), "lastName", "providerId");
		}

		// you can obtain profile information
		// System.out.println(profile.getFirstName());
		// OR also obtain list of contacts
		// List<Contact> contactsList = provider.getContactList();

		if (localUserName == null) {
			String msg = "Impossible to create object LocalUserInfo because Variable localUserName is null";
			LOG.info(msg);
			throw new Exception(msg);
		}
		else {
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

	/**
	 * Returns newLocalUserName if localUserName is null; localUserName
	 * otherwise.
	 * 
	 * @param localUserName
	 *            is the name of the user.
	 * @param newLocalUserName
	 *            is the new value proposed for localUserName.
	 * @return newLocalUserName if localUserName is null; localUserName
	 *         otherwise
	 * @throws LocalUserInfoException
	 * 
	 */
	private void ifNullThenSetUserNameFrom(String beforeAt, String afterAt, String msgForBeforeAt, String msgForAfterAt) {
		if (localUserName == null) {
			if ((beforeAt != null) && (afterAt != null)) {
				if (beforeAt.contains(afterAt)) {
					localUserName = fixLocalUserName(beforeAt);
					// LOG.info(msgForBeforeAt);
				} else {
					localUserName = fixLocalUserName(beforeAt + "_at_" + afterAt);
					// LOG.info(msgForBeforeAt+ "+ _at_ + "+ msgForAfterAt);
				}
			}
		}
	}

	/**
	 * Fixes invalid localUserNames.
	 * 
	 * @param localUserName
	 *            is the name of the user.
	 * @return the fixed localUserName or null if we cannot fix it.
	 * 
	 */
	private String fixLocalUserName(String newLocalUserName) {
		// String msg = "fixLocalUserName: ";
		if ((newLocalUserName != null) && (!"".equals(newLocalUserName))) {
			// msg += newLocalUserName + " -> ";
			newLocalUserName = newLocalUserName.replaceAll("\\s", "_");
			// msg += newLocalUserName + " -> ";
			newLocalUserName = newLocalUserName.replaceAll("\\@", "_at_");
			// msg += newLocalUserName + " -> ";
			newLocalUserName = newLocalUserName.replaceAll("\\.", "_");
			// msg += newLocalUserName + " ";
			// LOG.info(msg);
		}

		if (null != nullOnlyIfUserNameIsValid(newLocalUserName)) {
			newLocalUserName = null;
		}
		return newLocalUserName;
	}

	/**
	 * Checks if an user name is valid.
	 * 
	 * @param localUserName
	 *            is the name of the user that we are checking.
	 * @return null if the localUserName parameter serves as a valid user name.
	 */
	public static String nullOnlyIfUserNameIsValid(String userName) {

		if (userName == null) {
			return "userName is null";
		}
		if ("".equals(userName)) {
			return "userName is empty";
		}
		if (userName.contains("\\s")) {
			return "userName contains \\s";
		}
		if (userName.contains("\\@")) {
			return "userName contains \\@";
		}
		if (userName.contains("\\.")) {
			return "userName contains \\.";
		}
		if (userName.contains("/")) {
			return "userName contains /.";
		}

		return null;
	}
}

/*----*/
