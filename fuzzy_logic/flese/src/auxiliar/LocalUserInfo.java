package auxiliar;

import org.brickred.socialauth.Profile;

import storeHouse.SessionStoreHouse;

public class LocalUserInfo {

	private String localUserName = null;

	/**
	 * Tests if the client session has been authenticated, which is why it needs
	 * request and response.
	 * 
	 * @param request
	 *            is the HttpServletRequest
	 * @param response
	 *            is the HttpServletResponse
	 * @throws Exception
	 *             if request is null, response is null, session is null or
	 *             localUserName can not be set.
	 */
	public LocalUserInfo(SessionStoreHouse sessionStoreHouse) throws LocalUserInfoException {

		Profile profile = sessionStoreHouse.getUserProfile();
		if (profile == null) {
			ifNullThenSetUserNameFrom("Testing User", "localhost.localnet", "testing", "testing");
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

		if (localUserName == null)
			throw new LocalUserInfoException("localUserName is null");
		
		sessionStoreHouse.setLocalUserInfo(this);
	}

	public String setLocalUserName() {
		return localUserName;
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
	private void ifNullThenSetUserNameFrom(String beforeAt, String afterAt, String msgForBeforeAt, String msgForAfterAt) throws LocalUserInfoException {
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
	 * @return the fixed localUserName.
	 * @throws Exception
	 * @exception LocalUserNameFixesClassException
	 *                if localUserName is empty, null or can not be fixed.
	 * 
	 */
	private String fixLocalUserName(String newLocalUserName) throws LocalUserInfoException {
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
		if ("".equals(newLocalUserName))
			newLocalUserName = null;
		if (!checkUserNameIsValid(newLocalUserName))
			newLocalUserName = null;
		return newLocalUserName;
	}
	
	/**
	 * Checks if an user name is valid.
	 * 
	 * @param localUserName
	 *            is the name of the user that we are checking.
	 * @exception LocalUserNameFixesClassException
	 *                if localUserName is empty, null or invalid.
	 */
	public static boolean checkUserNameIsValid(String userName) throws LocalUserInfoException {

		if (userName == null)
			throw new LocalUserInfoException("userName is null");
		if ("".equals(userName))
			throw new LocalUserInfoException("userName is empty");
		if (userName.contains("\\s"))
			throw new LocalUserInfoException("userName contains \\s");
		if (userName.contains("\\@"))
			throw new LocalUserInfoException("userName contains \\@");
		if (userName.contains("\\."))
			throw new LocalUserInfoException("userName contains \\.");
		if (userName.contains("/"))
			throw new LocalUserInfoException("userName contains /.");

		return true;
	}
}

/*----*/
