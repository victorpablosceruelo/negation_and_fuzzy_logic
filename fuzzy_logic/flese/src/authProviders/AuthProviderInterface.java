package authProviders;

import storeHouse.RequestStoreHouse;

public interface AuthProviderInterface {

	public AuthenticationResult authenticationFirstStep() throws AuthProviderException;
	
	public AuthenticationResult authenticationCallback(RequestStoreHouse requestStoreHouse) throws Exception;
	
	public void deauthenticate();
	
	public String getLocalUserName(boolean appIsInTestingMode);
	
}
