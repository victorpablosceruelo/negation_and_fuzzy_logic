package auxiliar;

import java.util.ArrayList;

import javax.servlet.http.HttpServletRequest;

import programAnalysis.ProgramPartAnalysis;
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

	public static String getExceptionMsg(HttpServletRequest request) {
		ResultsStoreHouse resultsStoreHouse = JspsUtils.getResultsStoreHouse(request);
		String msg = resultsStoreHouse.getExceptionMsg();
		return msg;
	}

	public static String getExceptionMsgInJS(HttpServletRequest request) {
		String msg = getExceptionMsg(request);
		String[] msgsAux = { msg };
		return getMessagesInJS(msgsAux);
	}

	public static String getResultMessage(HttpServletRequest request) {
		ResultsStoreHouse resultsStoreHouse = JspsUtils.getResultsStoreHouse(request);
		String msg = resultsStoreHouse.getResultMessage();
		return msg;
	}

	public static String getResultMessageInJS(HttpServletRequest request) {
		String msg = getResultMessage(request);
		String[] msgsAux = { msg };
		return getMessagesInJS(msgsAux);
	}

	public static String getMessagesInJS(ArrayList<String> msgs) {
		String [] msgsAux = msgs.toArray(new String [msgs.size()]);
		String msg = getMessagesInJS(msgsAux);
		return msg;
	}
	
	public static String getMessagesInJS(String[] msgs) {

		StringBuilder msg = new StringBuilder();
		msg.append("new Array(");
		for (int i = 0; i < msgs.length; i++) {
			msg.append("'");
			if ((msgs[i] != null) && (!"".equals(msgs[i]))) {
				msg.append(msgs[i]);
			} else {
				msg.append(" ");
			}
			msg.append("'");
			if (i + 1 < msgs.length) {
				msg.append(", ");
			}
		}
		msg.append(")");
		return msg.toString();
	}

	public static String comboBoxDefaultValue() {
		return "<option id='----' title='----' value='----'>----</option>";
	}

	public static String getPrologNameInColloquialLanguage(String textIn) {

		if ((textIn == null) || ("".equals(textIn))) {
			return "";
		}

		textIn = fixNamesInSpecialCases(textIn);

		// debug.info("textIn: " + textIn);
		String text = null;
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

	private static String fixNamesInSpecialCases(String textIn) {
		if (textIn == null)
			return null;
		if ("".equals(textIn))
			return "";

		if ("fnot".equals(textIn))
			return "not";

		return textIn;
	}

	public static String humanizeIfTrue(String text, boolean humanize) {
		if (humanize) {
			return getPrologNameInColloquialLanguage(text);
		}
		return text;
	}

	public static String getFromFuzzificationNameOf(ProgramPartAnalysis programPartAnalysis, String element, boolean humanize) {
		if (programPartAnalysis == null) {
			return "";
		}

		String predDefined = programPartAnalysis.getPredDefined();
		String predNecessary = programPartAnalysis.getPredNecessary();

		int indexI1 = predDefined.indexOf("(");
		int indexJ1 = predDefined.indexOf(")");

		int indexI2 = predNecessary.indexOf("(");
		// int indexJ2 = predNecessary.indexOf(")");

		String databaseName1 = predDefined.substring(indexI1 + 1, indexJ1);
		// String databaseName2 = predDefined.substring(indexI2 + 1, indexJ2);

		predDefined = predDefined.substring(0, indexI1);
		predNecessary = predNecessary.substring(0, indexI2);

		if (element.equals(KConstants.Fuzzifications.predDefined)) {
			return humanizeIfTrue(predDefined, humanize);
		}
		if (element.equals(KConstants.Fuzzifications.predNecessary)) {
			return humanizeIfTrue(predNecessary, humanize);
		}
		if (element.equals(KConstants.Fuzzifications.database)) {
			return humanizeIfTrue(databaseName1, humanize);
		}

		return "";
	}

	public static String getValue(String value) {
		return value;
	}
}

// END