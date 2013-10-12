package auxiliar;

import javax.servlet.http.HttpServletRequest;

import results.ResultsStoreHouse;
import storeHouse.RequestStoreHouse;
import storeHouse.SessionStoreHouse;
import constants.KConstants;

public class JspsUtils {

	public static String getStringWithValueS() {
		return "S";
	}

	public static String getLocalUserInfoName(HttpServletRequest request) {

		String localUserInfoName = null;
		try {
			RequestStoreHouse requestStoreHouse = new RequestStoreHouse(request, false, true, true);
			localUserInfoName = requestStoreHouse.getSession().getLocalUserInfo().getLocalUserName();
		} catch (Exception e) {
			localUserInfoName = "";
		}
		return localUserInfoName;
	}
	
	public static SessionStoreHouse getSessionStoreHouse(HttpServletRequest request) {
		SessionStoreHouse sessionStoreHouse = null;
		try {
			RequestStoreHouse requestStoreHouse = new RequestStoreHouse(request);
			sessionStoreHouse = requestStoreHouse.getSession();
		} catch (Exception e) {
			sessionStoreHouse = null;
		}
		return sessionStoreHouse;
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

	public static String getPrologNameInColloquialLanguage(String textIn) {
		String text = null;

		if ((textIn == null) || ("".equals(textIn))) {
			return "";
		}

		// debug.info("textLabel: " + textLabelIn);
		int i = textIn.indexOf("_");
		while (i != -1) {
			text = ""; // Initialize
			text += textIn.substring(0, i);
			text += " ";
			text += textIn.substring(i + 1, textIn.length());
			// debug.info(textLabel);
			textIn = text;
			i = textIn.indexOf("_");
		}
		return textIn;
	}
	
	public static String getValue(String value) {
		return value;
	}
}
