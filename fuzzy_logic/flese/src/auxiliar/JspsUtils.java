package auxiliar;

import javax.servlet.http.HttpServletRequest;

import storeHouse.RequestStoreHouse;

public class JspsUtils {

	public static String getLocalUserInfoName(HttpServletRequest request, boolean nameOrEmpty) {

		String localUserInfoName = null;
		try {
			RequestStoreHouse requestStoreHouse = new RequestStoreHouse(request, false);
			localUserInfoName = requestStoreHouse.session.getLocalUserInfo().getLocalUserName();
		} catch (Exception e) {
			if (nameOrEmpty) {
				localUserInfoName = "";
			} else {
				localUserInfoName = "Not logged in";
			}
		}
		return localUserInfoName;
	}

}
