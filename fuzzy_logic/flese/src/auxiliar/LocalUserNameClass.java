package auxiliar;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.brickred.socialauth.AuthProvider;
import org.brickred.socialauth.Profile;
import org.brickred.socialauth.SocialAuthManager;

public class LocalUserNameClass {

	private static final Log LOG = LogFactory.getLog(LocalUserNameClass.class);
	private static String localUserName = null;
	
	/**
	 * Tests if the client session has been authenticated, which is why it needs request and response.
	 * 
	 * @param request is the HttpServletRequest
	 * @param response is the HttpServletResponse
	 * @throws Exception if request is null, response is null, session is null or localUserName can not be set.
	 */
	public LocalUserNameClass(HttpServletRequest request, HttpServletResponse response) throws Exception {
		
		if (request == null) throw new Exception("request is null");
		if (response == null) throw new Exception("response is null");
		
		HttpSession session = request.getSession(false);
		if (session == null) throw new Exception("session is null");

		String testingMode = (String) session.getAttribute("testingMode");
		Profile profile = null;
		if (testingMode == null) {
			SocialAuthManager manager = (SocialAuthManager) session.getAttribute("authManager");
			if (manager == null) throw new Exception("manager is null");

			AuthProvider provider = (AuthProvider) session.getAttribute("provider");
			if (provider == null) throw new Exception("provider is null");
			
			// get profile
			profile = provider.getUserProfile();
			if (profile == null) throw new Exception("profile is null");
			else {
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
		}
		else {
			ifNullThenSetUserNameFrom("Testing User", "wakamola.es", "testing", "testing");
		}
		if (localUserName == null) throw new Exception("localUserName is null");
		LOG.info("localUserName: " + localUserName);
		
		if (request.getAttribute("localUserName") != null) request.removeAttribute("localUserName");
		request.setAttribute("localUserName", localUserName);
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
					localUserName = fixLocalUserName(beforeAt + "_at_" + afterAt);
					LOG.info(msgForBeforeAt+ "+ _at_ + "+ msgForAfterAt);
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
