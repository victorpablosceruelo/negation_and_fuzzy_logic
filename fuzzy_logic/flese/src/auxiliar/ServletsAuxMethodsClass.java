package auxiliar;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Enumeration;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import urls.UrlMap;
import auxiliar.NextStep.Constants;
import constants.KPages;

public class ServletsAuxMethodsClass {

	final static Log LOG = LogFactory.getLog(ServletsAuxMethodsClass.class);

	/**
	 * Checks if an user name is valid.
	 * 
	 * @param localUserName
	 *            is the name of the user that we are checking.
	 * @exception LocalUserNameFixesClassException
	 *                if localUserName is empty, null or invalid.
	 */
	public static boolean checkUserNameIsValid(String userName) throws Exception {

		if (userName == null)
			throw new Exception("userName is null");
		if ("".equals(userName))
			throw new Exception("userName is empty");
		if (userName.contains("\\s"))
			throw new Exception("userName contains \\s");
		if (userName.contains("\\@"))
			throw new Exception("userName contains \\@");
		if (userName.contains("\\."))
			throw new Exception("userName contains \\.");
		if (userName.contains("/"))
			throw new Exception("userName contains /.");

		return true;
	}

	/**
	 * Executes default actions when an exception is thrown.
	 * 
	 * @param where
	 *            is the where we are redirected whe an exception occurs.
	 * @param addToUri
	 *            is the string added to the where, for adding additional
	 *            parameters to the request.
	 * @param e
	 *            is the exception thrown.
	 * @param request
	 *            is the HttpServletRequest.
	 * @param response
	 *            is the HttpServletResponse.
	 * @param LOG
	 *            is the Log facility. Can be null.
	 */
	static public void actionOnException(UrlMap urlMap, String addToUri, Exception e, HttpServletRequest request,
			HttpServletResponse response, Log LOG) {
		try {
			actionOnExceptionAux(urlMap, addToUri, e, request, response, LOG);
		} catch (java.lang.IllegalStateException e1) {
			LOG.error("-------------------------------------------------------------------");
			LOG.error("Exception thrown inside actionOnException: " + e);
			e.printStackTrace();
			LOG.error("-------------------------------------------------------------------");
		} catch (Exception e2) {
			LOG.error("-------------------------------------------------------------------");
			LOG.error("Exception thrown inside actionOnException: " + e);
			e.printStackTrace();
			LOG.error("-------------------------------------------------------------------");

			try {
				actionOnExceptionAux(KPages.SignOutRequest, "", e, request, response, LOG);
			} catch (Exception e3) {
				LOG.error("-------------------------------------------------------------------");
				LOG.error("Exception thrown inside actionOnException: " + e);
				e.printStackTrace();
				LOG.error("-------------------------------------------------------------------");
			}
		}
	}

	static private void actionOnExceptionAux(UrlMap urlMap, String addToUri, Exception e, HttpServletRequest request,
			HttpServletResponse response, Log LOG) throws Exception {
		if (e != null) {
			if (LOG != null) {
				LOG.error("Exception thrown: " + e);
				LOG.error("Exception trace: ");
			}
			e.printStackTrace();
		} else if (request == null)
			throw new Exception("request is null.");
		if (response == null)
			throw new Exception("response is null.");
		NextStep nextStep = new NextStep(Constants.forward_to, urlMap, addToUri);
		nextStep.takeAction(request, response);
	}

	public static String getCurrentDate() {
		DateFormat dateFormat = new SimpleDateFormat("yyyyMMdd_HHmmss");
		Date date = new Date();
		// System.out.println(dateFormat.format(date));
		return (dateFormat.format(date));
	}

	public static String requestParametersToString(HttpServletRequest request, Log LOG) {
		Enumeration<String> parametersEnum = request.getParameterNames();
		String parameterName;
		String retString = "";
		Integer counter = 0;

		while (parametersEnum.hasMoreElements()) {
			// Get the name of the request parameter
			parameterName = parametersEnum.nextElement().toString();
			LOG.info("Parameter name: " + parameterName);

			String[] values = request.getParameterValues(parameterName);
			for (int i = 0; i < values.length; i++) {
				LOG.info("Parameter name and value: " + parameterName + ": " + values[i]);
				if (counter == 0) {
					retString += "?";
				} else {
					retString += "&";
				}
				retString += parameterName + "=" + values[i];
				counter++;
			}
		}
		LOG.info("<end of attributes list>");
		return retString;
	}
}
