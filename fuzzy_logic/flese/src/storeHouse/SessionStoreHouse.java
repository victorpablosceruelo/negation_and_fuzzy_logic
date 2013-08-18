package storeHouse;

import java.util.Iterator;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.brickred.socialauth.AuthProvider;
import org.brickred.socialauth.Profile;
import org.brickred.socialauth.SocialAuthManager;
import org.brickred.socialauth.util.SocialAuthUtil;

import auxiliar.LocalUserInfo;

public class SessionStoreHouse {

	private HttpServletRequest request = null;
	private HttpServletResponse response = null;
	private HttpSession session = null;

	private static class Constants {
		public static class Session {
			public static String swAppInTestingMode = "swAppInTestingMode";
			public static String socialAuthManager = "socialAuthManager";
			public static String socialAuthProvider = "socialAuthProvider";
			public static String socialAuthProviderId = "socialAuthProviderId";
			public static String socialAuthProfile = "socialAuthProfile";
			public static String localUserInfo = "localUserInfo";

		}

		public static class Request {
			public static String operation = "op";
			public static String providerId = "id";
		}
	}

	public SessionStoreHouse(HttpServletRequest request, HttpServletResponse response, boolean create) throws Exception {

		if (request == null)
			throw new Exception("request is null");
		if (response == null)
			throw new Exception("response is null");

		this.request = request;
		this.response = response;
		session = request.getSession(create);
		if (session == null)
			throw new Exception("session is null. 'Create if null' has the value: " + create);
	}

	public void setAppInTestingMode(boolean value) {
		session.setAttribute(Constants.Session.swAppInTestingMode, value);
	}

	public boolean appIsInTestingMode() {
		String isInTestingMode = (String) session.getAttribute(Constants.Session.swAppInTestingMode);
		if ((isInTestingMode != null) && ("true".equals(isInTestingMode)))
			return true;
		return false;
	}

	public String getRequestOp() {
		String requestOp = request.getParameter(Constants.Request.operation);
		if (requestOp == null)
			return "";
		else
			return requestOp;
	}

	public void setRequestOp(String value) {
		request.setAttribute("op", value);
	}

	public String getRequestUrlString() {
		if (this.request.getRequestURL() == null)
			return null;
		return this.request.getRequestURL().toString();
	}

	public String getServerName() {
		return this.request.getServerName();
	}

	/**
	 * Adds a message to the request session attribute msgs.
	 * 
	 * @param msg
	 *            is the message to be added. Cannot be null.
	 * @param LOG
	 *            is the servlet logging facility. Can be null (but it is not
	 *            recommended).
	 */
	public void addMessageForTheUser(String msg) {

		String[] currentMsgs = (String[]) request.getAttribute("msgs");
		String[] newMsgs;
		if (currentMsgs != null) {
			newMsgs = new String[currentMsgs.length + 1];
			for (int i = 0; i < currentMsgs.length; i++) {
				newMsgs[i] = currentMsgs[i];
			}
			newMsgs[currentMsgs.length] = msg;
			// Remove the old messages array.
			request.removeAttribute("msgs");
		} else {
			newMsgs = new String[1];
			newMsgs[0] = msg;
		}
		// Save the new messages array.
		request.setAttribute("msgs", newMsgs);
		// Log
	}

	public void setSocialAuthManager(SocialAuthManager socialAuthManager) {

		session.removeAttribute(Constants.Session.socialAuthManager);
		session.setAttribute(Constants.Session.socialAuthManager, socialAuthManager);
	}

	public SocialAuthManager getSocialAuthManager() {
		SocialAuthManager socialAuthManager = (SocialAuthManager) session.getAttribute(Constants.Session.socialAuthManager);
		return socialAuthManager;
	}

	public String tryAuthenticationWithSocialAuthManager() throws Exception {
		SocialAuthManager socialAuthManager = (SocialAuthManager) session.getAttribute(Constants.Session.socialAuthManager);
		if (socialAuthManager == null)
			throw new Exception("Social Auth Manager is null");

		session.removeAttribute(Constants.Session.socialAuthManager);

		// call connect method of manager which returns the provider object.
		// Pass request parameter map while calling connect method.
		AuthProvider provider = null;
		try {
			provider = socialAuthManager.connect(SocialAuthUtil.getRequestParametersMap(request));
		} catch (Exception e) {
			provider = null;
			e.printStackTrace();
			throw new Exception("Error connecting social authentication provider.");
		}

		if (provider == null)
			throw new Exception("provider is null");

		// Retrieve the provider id to rebuild the initial query.
		// NO: authManager.getCurrentAuthProvider().getProviderId();
		String providerId = provider.getProviderId();
		if (providerId == null)
			throw new Exception("providerId is null");

		Profile profile = provider.getUserProfile();
		if (profile == null)
			throw new Exception("profile is null");

		// Save new computed results in session.
		session.setAttribute(Constants.Session.socialAuthManager, socialAuthManager);
		session.setAttribute(Constants.Session.socialAuthProvider, provider);
		session.setAttribute(Constants.Session.socialAuthProviderId, providerId);
		session.setAttribute(Constants.Session.socialAuthProfile, profile);

		return providerId;
	}

	public Profile getUserProfile() throws Exception {
		return (Profile) session.getAttribute(Constants.Session.socialAuthProfile);
	}

	public void setLocalUserInfo(LocalUserInfo localUserInfo) {
		if (request.getAttribute(Constants.Session.localUserInfo) != null)
			request.removeAttribute(Constants.Session.localUserInfo);
		request.setAttribute(Constants.Session.localUserInfo, localUserInfo);
	}

	public LocalUserInfo getLocalUserInfo() {
		return (LocalUserInfo) request.getAttribute(Constants.Session.localUserInfo);
	}

	public String getProviderId() throws Exception {
		String providerId = (String) session.getAttribute(Constants.Session.socialAuthProviderId);

		if ((providerId == null) || ("".equals(providerId)))
			providerId = (String) request.getParameter(Constants.Request.providerId);

		if ((providerId == null) || ("".equals(providerId)))
			throw new Exception("providerId is null in session and in request.");

		return providerId;
	}

	public void invalidateSession() {

		if (session != null) {
			SocialAuthManager authManager = (SocialAuthManager) session.getAttribute(Constants.Session.socialAuthManager);
			if (authManager != null) {
				List<String> connectedProvidersIds = authManager.getConnectedProvidersIds();
				if (connectedProvidersIds != null) {
					Iterator<String> connectedProvidersIdsIterator = connectedProvidersIds.iterator();
					if (connectedProvidersIdsIterator != null) {
						while (connectedProvidersIdsIterator.hasNext()) {
							String id = connectedProvidersIdsIterator.next();
							if (id != null) {
								authManager.disconnectProvider(id);
							}
						}
					}
				}
			}
			// Invalidate the session.
			session.invalidate();
		}
		
	}

}
