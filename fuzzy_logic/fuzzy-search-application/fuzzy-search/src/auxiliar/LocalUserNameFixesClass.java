package auxiliar;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.brickred.socialauth.Profile;

public class LocalUserNameFixesClass {

	private static final Log LOG = LogFactory.getLog(LocalUserNameFixesClass.class);
	
	/**
	 * Gets from profile a valid localUserName to identify uniquely the user logged in.
	 * 
	 * @param     profile is the profile returned by Open Authentication.
	 * @return    localUserName, an unique identifier for the logged in user.
	 * 
	 */
	public static String getLocalUserName(Profile profile) {
		String localUserName = null;
		if (profile != null) {
			
			localUserName = ifLocalUserNameNullReturnNewLocalUserName(localUserName, profile.getEmail());
			localUserName = ifLocalUserNameNullReturnNewLocalUserName(localUserName, profile.getDisplayName());

		}
		localUserName = ifLocalUserNameNullReturnNewLocalUserName(localUserName, "Testing User@wakamola.es");
		
		LOG.info("localUserName: " + localUserName);
		return localUserName;
	}

	/**
	 * Returns newLocalUserName if localUserName is null; localUserName otherwise.
	 * 
	 * @param     localUserName is the name of the user.
	 * @param     newLocalUserName is the new value proposed for localUserName.
	 * @return    newLocalUserName if localUserName is null; localUserName otherwise
	 * 
	 */
	public static String ifLocalUserNameNullReturnNewLocalUserName(String localUserName, String newLocalUserName) {
		if (localUserName == null) {
			if (newLocalUserName != null) {
				try {
					localUserName = fixLocalUserName(newLocalUserName);
				} catch (LocalUserNameFixesClassException e) {
					e.printStackTrace();
					localUserName = null;
				}
			}
		}
		return localUserName;
	}
	
	/**
	 * Fixes invalid localUserNames.
	 * 
	 * @param     localUserName is the name of the user.
	 * @return    the fixed localUserName.
	 * @exception LocalUserNameFixesClassException if localUserName is empty, null or can not be fixed.
	 * 
	 */
	private static String fixLocalUserName(String localUserName) 
			throws LocalUserNameFixesClassException {
		String fixedLocalUserName1, fixedLocalUserName2, fixedLocalUserName3;
		if ((localUserName == null) || "".equals(localUserName)) {
			throw new LocalUserNameFixesClassException("getWorkingFolder: localUserName can not be null nor empty string.");
		}
		else {
			fixedLocalUserName1 = localUserName.replaceAll("\\s", "_");
			fixedLocalUserName2 = fixedLocalUserName1.replaceAll("\\@", "_at_");
			fixedLocalUserName3 = fixedLocalUserName2.replaceAll("\\.", "_");
			LOG.info("fixedLocalUserName 1: " + fixedLocalUserName1 + " 2: " + fixedLocalUserName2 + " 3: " + fixedLocalUserName3);
		}
		// checkValidLocalUserName(fixedLocalUserName);
		return fixedLocalUserName3;
	}
	
	/**
	 * Checks if an username is valid.
	 * 
	 * @param     localUserName is the name of the user that we are checking.
	 * @exception LocalUserNameFixesClassException if localUserName is empty, null or invalid.
	 */
	public static void checkValidLocalUserName(String localUserName) throws LocalUserNameFixesClassException {
		if ((localUserName == null) || "".equals(localUserName)) {
			LOG.info("localUserName: " + localUserName);
			throw new LocalUserNameFixesClassException("getWorkingFolder: localUserName can not be null nor empty string. localUserName: " + localUserName);
		}
		
		if (! localUserName.equals(fixLocalUserName(localUserName))) {
			throw new LocalUserNameFixesClassException("getWorkingFolder: localUserName is not valid. localUserName: " + localUserName);
		}
		
		return;
	}
}
