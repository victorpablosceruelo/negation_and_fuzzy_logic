package authProviders;

public class Utils {

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
	public static String ifNullThenSetUserNameFrom(String localUserName, String beforeAt, String afterAt, String msgForBeforeAt,
			String msgForAfterAt) {
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
		return localUserName;
	}

	/**
	 * Fixes invalid localUserNames.
	 * 
	 * @param localUserName
	 *            is the name of the user.
	 * @return the fixed localUserName or null if we cannot fix it.
	 * 
	 */
	private static String fixLocalUserName(String newLocalUserName) {
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
