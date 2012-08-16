package auxiliar;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.brickred.socialauth.Profile;

public class LocalUserNameFixesClass {

	private static final Log LOG = LogFactory.getLog(LocalUserNameFixesClass.class);
	
	public static String getLocalUserName(Profile profile) {
		String localUserName = null;
		if (profile != null) {
			
			localUserName = ifLocalUserNameNullReturn(localUserName, profile.getEmail());
			localUserName = ifLocalUserNameNullReturn(localUserName, profile.getDisplayName());

		}
		localUserName = ifLocalUserNameNullReturn(localUserName, "Testing User@wakamola.es");
		
		LOG.info("localUserName: " + localUserName);
		return localUserName;
	}

	public static String ifLocalUserNameNullReturn(String localUserName, String newLocalUserName) {
		if (localUserName == null) {
			if (newLocalUserName != null) {
				try {
					localUserName = fixlocalUserName(newLocalUserName);
				} catch (LocalUserNameFixesClassException e) {
					e.printStackTrace();
					localUserName = null;
				}
			}
		}
		return localUserName;
	}
	
	private static String fixlocalUserName(String localUserName) 
			throws LocalUserNameFixesClassException {
		String fixedLocalUserName = null;
		if ((localUserName == null) || "".equals(localUserName)) {
			throw new LocalUserNameFixesClassException("getWorkingFolder: localUserName can not be null nor empty string.");
		}
		else {
			fixedLocalUserName = localUserName.replaceAll("\\s", "_");
			LOG.info("fixedLocalUserName: " + fixedLocalUserName);
			fixedLocalUserName = fixedLocalUserName.replaceAll("\\@", "_at_");
			LOG.info("fixedLocalUserName: " + fixedLocalUserName);
			fixedLocalUserName = fixedLocalUserName.replaceAll("\\.", "_");
			LOG.info("fixedLocalUserName: " + fixedLocalUserName);
		}
		return fixedLocalUserName;
	}
	
	public static void checkValidLocalUserName(String localUserName) throws LocalUserNameFixesClassException {
		if ((localUserName == null) || "".equals(localUserName)) {
			LOG.info("localUserName: " + localUserName);
			throw new LocalUserNameFixesClassException("getWorkingFolder: localUserName can not be null nor empty string. localUserName: " + localUserName);
		}
		return;
	}
}
