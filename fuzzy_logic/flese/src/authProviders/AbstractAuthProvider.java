package authProviders;

import storeHouse.RequestStoreHouse;

public abstract class AbstractAuthProvider implements AuthProviderInterface {

	private String authProviderId;

	protected AbstractAuthProvider(String authProviderId) {
		this.authProviderId = authProviderId;
	}
	
	public static AuthProviderInterface getInstance(String authProviderId) {
		if (KCtes.Providers.google.equals(authProviderId)) {
			return new OpenIdAuthProvider(authProviderId);
		}
		return null;
	}
	
	protected String getAuthProviderId() {
		return this.authProviderId;
	}

	public abstract AuthenticationResult authenticationFirstStep() throws AuthProviderException;

	public abstract AuthenticationResult authenticationCallback(RequestStoreHouse requestStoreHouse) throws Exception;

	public abstract void deauthenticate();

	public abstract String getLocalUserName(boolean appIsInTestingMode);

}
