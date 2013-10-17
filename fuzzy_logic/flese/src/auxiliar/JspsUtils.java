package auxiliar;

import java.util.ArrayList;

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

	public static ArrayList<String> getMessagesArrayList(HttpServletRequest request, ArrayList<String> msgs) {
		if (msgs == null) {
			msgs = new ArrayList<String>();
		}

		ResultsStoreHouse resultsStoreHouse = JspsUtils.getResultsStoreHouse(request);
		String[] msgsError = resultsStoreHouse.getMessages();
		if (msgsError != null) {
			for (int i = 0; i < msgsError.length; i++) {
				msgs.add(msgsError[i]);
			}
		}
		return msgs;
	}
	
	public static String [] getMessages(HttpServletRequest request, ArrayList<String> msgs) {
		msgs = getMessagesArrayList(request, msgs);
		return msgs.toArray(new String [msgs.size()]);
	}

	public static String getMessagesInJS(HttpServletRequest request, ArrayList<String> msgs) {
		String [] msgsAux = getMessages(request, msgs);

		StringBuilder msg = new StringBuilder();
		// msg.append("[");
		for (int i = 0; i < msgsAux.length; i++) {
			msg.append("'");
			msg.append(msgsAux[i]);
			msg.append("'");
			if (i + 1 < msgsAux.length) {
				msg.append(", ");
			}
		}
		// msg.append("]");
		return msg.toString();
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
	
	public static String getPrologNameAsFuzzificationFunctionName(String textIn, boolean full) {
		
		int indexI = textIn.indexOf("(");
		int indexJ = textIn.indexOf(")");
		
		String predicateName = textIn.substring(indexI + 1, indexJ);
		String fuzzificationName = textIn.substring(0, indexI);
		
		if (full) {
			return getPrologNameInColloquialLanguage(predicateName) + " is " + getPrologNameInColloquialLanguage(fuzzificationName);
		}
		return getPrologNameInColloquialLanguage(fuzzificationName);
	}

	public static String getValue(String value) {
		return value;
	}
}




// END