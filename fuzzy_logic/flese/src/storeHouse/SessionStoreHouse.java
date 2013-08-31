package storeHouse;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.brickred.socialauth.AuthProvider;
import org.brickred.socialauth.Profile;
import org.brickred.socialauth.SocialAuthManager;

import auxiliar.LocalUserInfo;
import constants.KConstants;

public class SessionStoreHouse {

	private HttpSession session = null;

	public SessionStoreHouse(HttpServletRequest request, boolean create) throws RequestStoreHouseException {
		this.session = request.getSession(create);
		if (session == null)
			throw new RequestStoreHouseException("session is null. 'Create if null' has the value: " + create);
	}

	public boolean isNull() {
		return (this.session == null);
	}

	public void invalidateSession() {
		this.session.invalidate();
		this.session = null;
	}
	
	public void setAppInTestingMode(boolean value) {
		session.removeAttribute(KConstants.Session.swAppInTestingMode);
		session.setAttribute(KConstants.Session.swAppInTestingMode, value);
	}

	public boolean appIsInTestingMode() {
		String isInTestingMode = (String) session.getAttribute(KConstants.Session.swAppInTestingMode);
		if ((isInTestingMode != null) && ("true".equals(isInTestingMode)))
			return true;
		return false;
	}

	public void setSocialAuthManager(SocialAuthManager socialAuthManager) {
		session.removeAttribute(KConstants.Session.socialAuthManager);
		if (socialAuthManager != null)
			session.setAttribute(KConstants.Session.socialAuthManager, socialAuthManager);
	}

	public SocialAuthManager getSocialAuthManager() {
		SocialAuthManager socialAuthManager = (SocialAuthManager) session.getAttribute(KConstants.Session.socialAuthManager);
		return socialAuthManager;
	}

	public void setUserProfile(Profile profile) {
		session.removeAttribute(KConstants.Session.socialAuthProfile);
		if (profile != null)
			session.setAttribute(KConstants.Session.socialAuthProfile, profile);
	}

	public Profile getUserProfile() {
		Profile profile = (Profile) session.getAttribute(KConstants.Session.socialAuthProfile); 
		return profile;
	}

	public void setLocalUserInfo(LocalUserInfo localUserInfo) {
		session.removeAttribute(KConstants.Session.localUserInfo);
		if (localUserInfo != null)
			session.setAttribute(KConstants.Session.localUserInfo, localUserInfo);
	}
	
	public LocalUserInfo getLocalUserInfo() {
		LocalUserInfo localUserInfo = (LocalUserInfo) session.getAttribute(KConstants.Session.localUserInfo); 
		return localUserInfo;
	}

	public void setAuthProvider(AuthProvider authProvider) {
		session.removeAttribute(KConstants.Session.socialAuthProvider);
		if (authProvider != null)
			session.setAttribute(KConstants.Session.socialAuthProvider, authProvider);
	}
	
	public AuthProvider getAuthProvider() {
		AuthProvider authProvider = (AuthProvider) session.getAttribute(KConstants.Session.socialAuthProvider);
		return authProvider;
	}
	
	public void setProviderId(String providerId) {
		session.removeAttribute(KConstants.Session.socialAuthProviderId);
		if (providerId != null)
			session.setAttribute(KConstants.Session.socialAuthProviderId, providerId);
	}
	
	public String getProviderId() {
		String providerId = (String) session.getAttribute(KConstants.Session.socialAuthProviderId);
		return (providerId == null) ? "" : providerId;
	}

}
