package storeHouse;

import java.util.Enumeration;
import java.util.HashMap;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.fileupload.servlet.ServletFileUpload;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import auxiliar.LocalUserInfoException;
import constants.KConstants;
import filesAndPaths.FilesAndPathsException;
import filesAndPaths.ProgramFileInfo;

public class RequestStoreHouse {

	final Log LOG = LogFactory.getLog(RequestStoreHouse.class);
	private HttpServletRequest request = null;
	private HttpServletResponse response = null;
	private ServletContext servletContext = null;
	private String doMethod = null;
	private SessionStoreHouse session = null;

	private HashMap<String, String[]> requestParams = null;

	public RequestStoreHouse(HttpServletRequest request, boolean create) throws RequestStoreHouseException, RequestStoreHouseSessionException {

		if (request == null)
			throw new RequestStoreHouseException("request is null");

		this.request = request;
		session = new SessionStoreHouse(request, create);
		copyRequestParameters();
	}

	public HttpServletRequest getRequest() {
		return request;
	}

	public void setResponse(HttpServletResponse response) throws RequestStoreHouseException {
		if (response == null)
			throw new RequestStoreHouseException("response is null");
		this.response = response;
	}

	public HttpServletResponse getResponse() throws RequestStoreHouseException {
		if (this.response == null)
			throw new RequestStoreHouseException("response is null");
		return this.response;
	}

	public SessionStoreHouse getSession() throws RequestStoreHouseException {
		if (this.session == null)
			throw new RequestStoreHouseException("session is null");
		return this.session;
	}

	public void setServletContext(ServletContext servletContext) throws RequestStoreHouseException {
		if (servletContext == null)
			throw new RequestStoreHouseException("servletContext is null");

		this.servletContext = servletContext;
	}

	public ServletContext getServletContext() throws RequestStoreHouseException {
		if (this.servletContext == null)
			throw new RequestStoreHouseException("servletContext is null");
		return servletContext;
	}

	public void setDoMethod(String doMethod) throws RequestStoreHouseException {
		if (doMethod == null)
			throw new RequestStoreHouseException("doMethod is null");
		if ((!"doGet".equals(doMethod)) && (!"doPost".equals(doMethod)))
			throw new RequestStoreHouseException("doMethod is not doGet nor doPost.");

		this.doMethod = doMethod;
	}

	public String getDoMethod() throws RequestStoreHouseException {
		if (this.doMethod == null)
			throw new RequestStoreHouseException("doMethod is null");
		if ((!"doGet".equals(this.doMethod)) && (!"doPost".equals(this.doMethod)))
			throw new RequestStoreHouseException("doMethod is not doGet nor doPost.");

		return this.doMethod;
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	private void copyRequestParameters() {

		this.requestParams = new HashMap<String, String[]>();

		String parameterName = null;
		String[] valuesArrayIn = null;
		String[] valuesArrayTmp = null;
		StringBuilder logMsg = new StringBuilder();

		if (this.request != null) {
			// Get the values of all request parameters
			Enumeration<String> parametersEnum = this.request.getParameterNames();

			if (parametersEnum != null) {
				while (parametersEnum.hasMoreElements()) {
					// Get the name of the request parameter
					parameterName = (parametersEnum.nextElement()).toString();
					valuesArrayIn = this.request.getParameterValues(parameterName);
					logMsg.append("\n");
					logMsg.append(parameterName);
					logMsg.append(" = ");
					if (valuesArrayIn != null) {
						valuesArrayTmp = new String[valuesArrayIn.length];
						int j = 0;
						for (int i = 0; i < valuesArrayIn.length; i++) {
							if (i != 0)
								logMsg.append(", ");
							logMsg.append(valuesArrayIn[i]);
							if ((valuesArrayIn[i] != null) && (!"".equals(valuesArrayIn[i]))) {
								valuesArrayTmp[j] = valuesArrayIn[i];
								j++;
							}
						}
						if (j != 0) {
							this.requestParams.put(parameterName, valuesArrayTmp);
						}
					}
					this.request.removeAttribute(parameterName);
				}
			}
		}
		LOG.info("copyRequestParameters: " + logMsg.toString());
	}

	public boolean requestIsMultipartContent() {
		boolean isMultipart = ServletFileUpload.isMultipartContent(request);
		return isMultipart;
	}

	public String getRequestParameter(String paramName) {
		String[] values = requestParams.get(paramName);
		if (values != null) {
			if (values.length > 0)
				return values[0];
		}
		return "";
	}

	public String getRequestUrlString() {
		if (this.request.getRequestURL() == null)
			return null;
		return this.request.getRequestURL().toString();
	}

	public String getRequestServerName() {
		return this.request.getServerName();
	}

	public String getProviderId() throws RequestStoreHouseException {
		String providerId = "";
		if (session != null)
			providerId = session.getProviderId();

		if ((providerId == null) || ("".equals(providerId))) {
			providerId = (String) request.getParameter(KConstants.Request.providerId);
			if (session != null)
				session.setProviderId(providerId);
		}

		if ((providerId == null) || ("".equals(providerId)))
			throw new RequestStoreHouseException("providerId is null in session and in request.");

		return providerId;
	}

	public ProgramFileInfo getProgramFileInfo() throws FilesAndPathsException, LocalUserInfoException {
		String fileName = getRequestParameter(KConstants.Request.fileNameParam);
		String fileOwner = getRequestParameter(KConstants.Request.fileOwnerParam);

		return new ProgramFileInfo(fileOwner, fileName);
	}

}
