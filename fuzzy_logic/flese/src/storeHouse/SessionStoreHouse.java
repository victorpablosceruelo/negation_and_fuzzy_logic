package storeHouse;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.brickred.socialauth.AuthProvider;

import authProviders.AuthProviderInterface;
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

	public void setLocalUserInfo(LocalUserInfo localUserInfo) {
		session.removeAttribute(KConstants.Session.localUserInfo);
		if (localUserInfo != null)
			session.setAttribute(KConstants.Session.localUserInfo, localUserInfo);
	}

	public LocalUserInfo getLocalUserInfo() {
		LocalUserInfo localUserInfo = (LocalUserInfo) session.getAttribute(KConstants.Session.localUserInfo);
		return localUserInfo;
	}

	public void setAuthProvider(AuthProviderInterface authProvider) {
		session.removeAttribute(KConstants.Session.authProvider);
		if (authProvider != null)
			session.setAttribute(KConstants.Session.authProvider, authProvider);
	}

	public AuthProviderInterface getAuthProvider() {
		AuthProviderInterface authProvider = (AuthProviderInterface) session.getAttribute(KConstants.Session.authProvider);
		return authProvider;
	}

	public void setAuthProviderId(String providerId) {
		session.removeAttribute(KConstants.Session.authProviderId);
		if (providerId != null)
			session.setAttribute(KConstants.Session.authProviderId, providerId);
	}

	public String getAuthProviderId() {
		String providerId = (String) session.getAttribute(KConstants.Session.authProviderId);
		return (providerId == null) ? "" : providerId;
	}

	public void addToRegistryStoreHouse(RegistryEntry registryEntry) {
		try {
			addToRegistryStoreHouseAux(registryEntry);
		} catch (Throwable e) {
			e.printStackTrace();
		}
	}

	private synchronized void addToRegistryStoreHouseAux(RegistryEntry registryEntry) {
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

	public String[] getRegistryStoreHouse() {
		RegistryStoreHouse registryStoreHouse = privateGetRegistryStoreHouse();
		String[] registryEntries = new String[0];
		if (registryStoreHouse != null) {
			registryEntries = registryStoreHouse.getRegistryEntries();
		}
		return registryEntries;
	}
}
