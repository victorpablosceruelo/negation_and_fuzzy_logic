package authProviders;

import storeHouse.RequestStoreHouse;

public abstract class AbstractAuthProvider implements AuthProviderInterface {

	public static AuthProviderInterface getInstance(String authProviderId) {
		if (KCtes.Providers.google.equals(authProviderId)) {
			return new OpenIdAuthProvider();
		}
		return null;
	}

	public abstract AuthenticationResult authenticationFirstStep() throws Exception;

	public abstract AuthenticationResult authenticationCallback(RequestStoreHouse requestStoreHouse) throws Exception;

	public abstract void deauthenticate();

	public abstract String getLocalUserName(boolean appIsInTestingMode);

}
