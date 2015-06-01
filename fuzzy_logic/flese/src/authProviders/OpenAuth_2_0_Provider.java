package authProviders;

import org.apache.oltu.oauth2.client.OAuthClient;
import org.apache.oltu.oauth2.client.URLConnectionClient;
import org.apache.oltu.oauth2.client.request.OAuthClientRequest;
import org.apache.oltu.oauth2.client.response.GitHubTokenResponse;
import org.apache.oltu.oauth2.client.response.OAuthAuthzResponse;
import org.apache.oltu.oauth2.common.OAuthProviderType;
import org.apache.oltu.oauth2.common.exception.OAuthSystemException;
import org.apache.oltu.oauth2.common.message.types.GrantType;

import storeHouse.RequestStoreHouse;
import auxiliar.NextStep;
import constants.KConstants;

public class OpenAuth_2_0_Provider extends AbstractAuthProvider implements AuthProviderInterface {

	// The contents of this auth provider came from
	// https://cwiki.apache.org/confluence/display/OLTU/OAuth+2.0+Client+Quickstart

	String localUserName;

	protected OpenAuth_2_0_Provider(String authProviderId) {
		super(authProviderId);
		localUserName = "unknown@nowhere.org";
	}

	@Override
	public AuthenticationResult authenticationFirstStep(String callbackURL) throws AuthProviderException {
		OAuthClientRequest oauthRequest;
		try {
			oauthRequest = OAuthClientRequest.authorizationProvider(OAuthProviderType.FACEBOOK)
					.setClientId("your-facebook-application-client-id").setRedirectURI(callbackURL).buildQueryMessage();
		} catch (OAuthSystemException e) {
			e.printStackTrace();
			oauthRequest = null;
		}

		String nextURL = oauthRequest.getLocationUri();
		AuthenticationResult authResult = new AuthenticationResult();
		NextStep nextStep = new NextStep(KConstants.NextStep.redirect_to, null, nextURL);
		authResult.setNextStep(nextStep);
		return authResult;
	}

	@Override
	public AuthenticationResult authenticationCallback(RequestStoreHouse requestStoreHouse) throws Exception {
		OAuthAuthzResponse oar = OAuthAuthzResponse.oauthCodeAuthzResponse(requestStoreHouse.getRequest());
		String code = oar.getCode();

		OAuthClientRequest request = OAuthClientRequest.tokenProvider(OAuthProviderType.FACEBOOK)
				.setGrantType(GrantType.AUTHORIZATION_CODE).setClientId("your-facebook-application-client-id")
				.setClientSecret("your-facebook-application-client-secret").setRedirectURI("http://www.example.com/redirect").setCode(code)
				.buildQueryMessage();

		// create OAuth client that uses custom http client under the hood
		OAuthClient oAuthClient = new OAuthClient(new URLConnectionClient());

		// Facebook is not fully compatible with OAuth 2.0 draft 10, access
		// token response is
		// application/x-www-form-urlencoded, not json encoded so we use
		// dedicated response class for that
		// Custom response classes are an easy way to deal with oauth providers
		// that introduce modifications to
		// OAuth 2.0 specification
		GitHubTokenResponse oAuthResponse = oAuthClient.accessToken(request, GitHubTokenResponse.class);

		String accessToken = oAuthResponse.getAccessToken();
		Long expiresIn = oAuthResponse.getExpiresIn();
		// oAuthResponse.getParam(param)

		localUserName = oAuthResponse.getBody();

		return null;
	}

	@Override
	public void deauthenticate() {
		return;
	}

	@Override
	public String getLocalUserName(boolean appIsInTestingMode) {
		return localUserName;
	}
}
