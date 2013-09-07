package auxiliar;

import javax.servlet.http.HttpServletRequest;

import constants.KConstants;
import results.ResultsStoreHouse;
import storeHouse.RequestStoreHouse;

public class JspsUtils {

	public static String getLocalUserInfoName(HttpServletRequest request) {

		String localUserInfoName = null;
		try {
			RequestStoreHouse requestStoreHouse = new RequestStoreHouse(request, false);
			localUserInfoName = requestStoreHouse.getSession().getLocalUserInfo().getLocalUserName();
		} catch (Exception e) {
			localUserInfoName = "";
		}
		return localUserInfoName;
	}
	
	public static ResultsStoreHouse getResultsStoreHouse(HttpServletRequest request) {
		ResultsStoreHouse resultsStoreHouse = (ResultsStoreHouse) request.getAttribute(KConstants.Request.resultsStoreHouse);
		if (resultsStoreHouse == null) {
			resultsStoreHouse = new ResultsStoreHouse();
		}
		return resultsStoreHouse;
	}
	
	public static String comboBoxDefaultValue() {
		return "<option id='----' title='----' value='----'>----</option>";
	}
}
