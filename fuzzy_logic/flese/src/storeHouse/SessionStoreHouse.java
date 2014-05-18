package storeHouse;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.brickred.socialauth.AuthProvider;
import org.brickred.socialauth.Profile;
import org.brickred.socialauth.SocialAuthManager;

import auxiliar.LocalUserInfo;
import auxiliar.RegistryEntry;
import constants.KConstants;

public class SessionStoreHouse {

	private HttpSession session = null;

	public SessionStoreHouse(HttpServletRequest request, boolean create, boolean exceptionIfSessionIsNull,
			boolean exceptionIfLocalUserInfoIsNull) throws RequestStoreHouseException, RequestStoreHouseSessionException {
		this.session = request.getSession(create);
		if ((session == null) && (exceptionIfSessionIsNull)) {
			throw new RequestStoreHouseSessionException("session is null. 'Create if null' has the value: " + create);
		}
		if ((exceptionIfLocalUserInfoIsNull) && (getLocalUserInfo() == null)) {
			throw new RequestStoreHouseSessionException("Invalid session. Please Log In again");
		}
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
		session.setAttribute(KConstants.Session.swAppInTestingMode, value ? "true" : "false");
	}

	public boolean appIsInTestingMode() {
		String isInTestingMode = (String) session.getAttribute(KConstants.Session.swAppInTestingMode);
		if (isInTestingMode == null)
			return false;
		return "true".equals(isInTestingMode);
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

	public synchronized void addToRegistryStoreHouse(RegistryEntry registryEntry) {
		RegistryStoreHouse registryStoreHouse = privateGetRegistryStoreHouse();
		if (session != null) {
			session.removeAttribute(KConstants.Session.registryStoreHouse);
		}
		if (registryStoreHouse == null) {
			registryStoreHouse = new RegistryStoreHouse();
		}
		registryStoreHouse.addRegistryEntry(registryEntry);
		if ((registryStoreHouse != null) && (session != null)) {
			session.setAttribute(KConstants.Session.registryStoreHouse, registryStoreHouse);
		}
	}

	private RegistryStoreHouse privateGetRegistryStoreHouse() {
		RegistryStoreHouse registryStoreHouse = null;
		if (session != null)
			registryStoreHouse = (RegistryStoreHouse) session.getAttribute(KConstants.Session.registryStoreHouse);
		return (registryStoreHouse == null) ? null : registryStoreHouse;
	}
	
	public String [] getRegistryStoreHouse() {
		RegistryStoreHouse registryStoreHouse = privateGetRegistryStoreHouse();
		String [] registryEntries = new String[0];
		if (registryStoreHouse != null) {
			registryStoreHouse.getRegistryEntries();
		}
		return registryEntries;
	}
}
