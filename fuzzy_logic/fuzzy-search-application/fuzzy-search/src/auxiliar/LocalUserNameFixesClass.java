package auxiliar;

import org.brickred.socialauth.Profile;

public class LocalUserNameFixesClass {

	public static String getLocalUserName(Profile profile) {
		String localUserName = null;
		
		localUserName = ifLocalUserNameNullReturn(localUserName, profile.getEmail());
		localUserName = ifLocalUserNameNullReturn(localUserName, profile.getDisplayName());
		
		localUserName = ifLocalUserNameNullReturn(localUserName, "Testing User@wakamola.es");
		
		return localUserName;
	}

	public static String ifLocalUserNameNullReturn(String localUserName, String newLocalUserName) {
		if (localUserName != null) {
			if (newLocalUserName != null) {
				try {
					localUserName = fixlocalUserName(newLocalUserName);
				} catch (LocalUserNameFixesClassException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
					localUserName = null;
				}
			}
		}
		return localUserName;
	}
	
	private static String fixlocalUserName(String localUserName) 
			throws LocalUserNameFixesClassException {
		String fixedlocalUserName = null;
		if ((localUserName == null) || "".equals(localUserName)) {
			throw new LocalUserNameFixesClassException("getWorkingFolder: localUserName can not be null nor empty string.");
		}
		else {
			fixedlocalUserName = localUserName.replaceAll("\\s", "_");
			fixedlocalUserName = localUserName.replaceAll("@", "_at_");
			fixedlocalUserName = localUserName.replaceAll(".", "_");
		}
		return fixedlocalUserName;
	}
	
	public static void checkValidLocalUserName(String localUserName) throws LocalUserNameFixesClassException {
		if ((localUserName == null) || "".equals(localUserName)) {
			throw new LocalUserNameFixesClassException("getWorkingFolder: localUserName can not be null nor empty string.");
		}
		return;
	}
}
