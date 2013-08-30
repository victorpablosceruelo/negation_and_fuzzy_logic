package auxiliar;

import java.io.IOException;

import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import constants.KConstants;
import constants.KUrls;

import servlets.Servlet;
import urls.AppUrl;
import urls.UrlMap;

public class NextStep {

	final Log LOG = LogFactory.getLog(Servlet.class);

	public static class Constants {
		public final static int none = 0;
		public final static int forward_to = 1;
		public final static int redirect_to = 2;
		public final static int redirect_to_with_session = 3;
		public final static int sendRedirect_to = 4;
		public final static int invalidAction = 5;
	}

	private int action = 0;
	private UrlMap url = null;
	private String appended = "";

	public NextStep(int action, UrlMap url, String append) {
		if ((action <= Constants.none) || (action >= Constants.invalidAction))
			action = Constants.forward_to;
		if (url == null)
			url = KUrls.Pages.Exception;

		this.url = url;
		this.action = action;
		if (append != null)
			this.appended = append;
	}

	public void takeAction(HttpServletRequest request, HttpServletResponse response) throws NextStepException, IOException, ServletException {

		
		String isAjaxParam = request.getParameter(KConstants.Request.isAjaxParam);
		boolean isAjax = ((isAjaxParam != null) && (KConstants.Values.True.equals(isAjaxParam))); 

		String auxUrl = getUrl(isAjax);
		String auxFullUrl = getFullUrl(request, response, isAjax);

		switch (this.action) {
		case Constants.sendRedirect_to:
			LOG.info("sendRedirect_to: " + auxUrl);
			response.sendRedirect(auxUrl);
			break;
		case Constants.forward_to:
			LOG.info("forward_to: " + auxUrl);
			RequestDispatcher dispatcher = request.getRequestDispatcher(auxUrl);
			dispatcher.forward(request, response);
			break;
		case Constants.redirect_to:
			LOG.info("redirect_to: " + auxFullUrl);
			response.sendRedirect(auxFullUrl);
			break;
		case Constants.redirect_to_with_session:
			LOG.info("redirect_to_with_session: " + auxFullUrl);
			response.encodeRedirectURL(auxFullUrl);
			break;
		case Constants.none:
		default:
			LOG.info("No action to take. ");
		}
	}

	public String getUrl(boolean isAjax) {
		return url.getUrl(isAjax) + appended;
	}
	
	public String getFullUrl(HttpServletRequest request, HttpServletResponse response, boolean isAjax) throws NextStepException {
		if (request == null)
			throw new NextStepException("request is null.");
		if (response == null)
			throw new NextStepException("response is null.");
		
		String requestUrl = request.getRequestURL().toString();
		String serverName = request.getServerName();
		
		String appUrl = AppUrl.getAppUrl(requestUrl, serverName);
		return appUrl + getUrl(isAjax);
	}
	
}
