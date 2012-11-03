package auxiliar;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.brickred.socialauth.Profile;

public class LocalUserNameClass {

	private static final Log LOG = LogFactory.getLog(LocalUserNameClass.class);
	private static String localUserName = null;
	
	/**
	 * Gets from profile a valid localUserName to identify uniquely the user logged in.
	 * 
	 * @param     profile is the profile returned by Open Authentication.
	 * @return    localUserName, an unique identifier for the logged in user.
	 * @throws Exception 
	 * 
	 */
	public LocalUserNameClass(Profile profile) throws Exception {
		if (profile != null) { 
				ifNullThenSetUserNameFrom(profile.getEmail(), profile.getProviderId(), "email", "providerId"); 
				ifNullThenSetUserNameFrom(profile.getDisplayName(), profile.getProviderId(), "displayName", "providerId");
				ifNullThenSetUserNameFrom(profile.getFullName(), profile.getProviderId(), "fullName", "providerId");
				ifNullThenSetUserNameFrom(profile.getFirstName(), profile.getProviderId(), "firstName", "providerId");
				ifNullThenSetUserNameFrom(profile.getLastName(), profile.getProviderId(), "lastName", "providerId");
		}
		ifNullThenSetUserNameFrom("Testing User", "wakamola.es", "testing", "testing");
		LOG.info("localUserName: " + localUserName);
	}
	
	public String getLocalUserName() {
		return localUserName;
	}

	/**
	 * Returns newLocalUserName if localUserName is null; localUserName otherwise.
	 * 
	 * @param     localUserName is the name of the user.
	 * @param     newLocalUserName is the new value proposed for localUserName.
	 * @return    newLocalUserName if localUserName is null; localUserName otherwise
	 * @throws Exception 
	 * 
	 */
	private void ifNullThenSetUserNameFrom(String beforeAt, String afterAt, String msgForBeforeAt, String msgForAfterAt) 
			throws Exception {
		if (localUserName == null) {
			if ((beforeAt != null) && (afterAt != null)){
				if (beforeAt.contains(afterAt)) {
					localUserName = fixLocalUserName(beforeAt);
					LOG.info(msgForBeforeAt);
				}
				else {
					localUserName = fixLocalUserName(beforeAt + afterAt);
					LOG.info(msgForBeforeAt+ " + " + msgForAfterAt);
				}
			}
		}
	}
	
	/**
	 * Fixes invalid localUserNames.
	 * 
	 * @param     localUserName is the name of the user.
	 * @return    the fixed localUserName.
	 * @throws Exception 
	 * @exception LocalUserNameFixesClassException if localUserName is empty, null or can not be fixed.
	 * 
	 */
	private String fixLocalUserName(String newLocalUserName) throws Exception {
		String msg = "fixLocalUserName: ";
		if ((newLocalUserName != null) && (! "".equals(newLocalUserName))) {
			msg += newLocalUserName + " -> ";
			newLocalUserName = newLocalUserName.replaceAll("\\s", "_");
			msg += newLocalUserName + " -> ";
			newLocalUserName = newLocalUserName.replaceAll("\\@", "_at_");
			msg += newLocalUserName + " -> ";
			newLocalUserName = newLocalUserName.replaceAll("\\.", "_");
			msg += newLocalUserName + " ";
			LOG.info(msg);
		}
		if ("".equals(newLocalUserName)) newLocalUserName=null;
		if (! ServletsAuxMethodsClass.checkUserNameIsValid(newLocalUserName)) newLocalUserName=null;
		return newLocalUserName;
	}
}




/*----*/
