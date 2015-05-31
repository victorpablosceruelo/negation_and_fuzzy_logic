package authProviders;

import storeHouse.RequestStoreHouse;
import auxiliar.NextStep;
import constants.KConstants;

public class OpenAuth_2_0_Provider extends AbstractAuthProvider implements AuthProviderInterface {

	// The contents of this auth provider came from
	// https://cwiki.apache.org/confluence/display/OLTU/OAuth+2.0+Client+Quickstart

	protected OpenAuth_2_0_Provider(String authProviderId) {
		super(authProviderId);
	}

	@Override
	public AuthenticationResult authenticationFirstStep(String callbackURL) throws AuthProviderException {
		OAuthClientRequest oauthRequest = OAuthClientRequest.authorizationProvider(OAuthProviderType.FACEBOOK)
				.setClientId("your-facebook-application-client-id").setRedirectURI(callbackURL).buildQueryMessage();

		String nextURL = oauthRequest.getLocationUri();
		AuthenticationResult authResult = new AuthenticationResult();
		NextStep nextStep = new NextStep(KConstants.NextStep.redirect_to, null, nextURL);
		authResult.setNextStep(nextStep);
		return authResult;
	}

	@Override
	public AuthenticationResult authenticationCallback(RequestStoreHouse requestStoreHouse) throws Exception {
		return null;
	}

	@Override
	public void deauthenticate() {
		return;
	}

	@Override
	public String getLocalUserName(boolean appIsInTestingMode) {
		return "unknown@fakeAuthProv.org";
	}
}
