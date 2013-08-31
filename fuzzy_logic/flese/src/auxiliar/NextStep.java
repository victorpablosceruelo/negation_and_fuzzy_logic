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

	private int action = 0;
	private UrlMap url = null;
	private String appended = "";

	public NextStep(int action, UrlMap url, String append) {
		if ((action <= KConstants.NextStep.none) || (action >= KConstants.NextStep.invalidAction))
			action = KConstants.NextStep.forward_to;
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
		case KConstants.NextStep.sendRedirect_to:
			LOG.info("sendRedirect_to: " + auxUrl);
			response.sendRedirect(auxUrl);
			break;
		case KConstants.NextStep.forward_to:
			LOG.info("forward_to: " + auxUrl);
			RequestDispatcher dispatcher = request.getRequestDispatcher(auxUrl);
			dispatcher.forward(request, response);
			break;
		case KConstants.NextStep.redirect_to:
			LOG.info("redirect_to: " + auxFullUrl);
			response.sendRedirect(auxFullUrl);
			break;
		case KConstants.NextStep.redirect_to_with_session:
			LOG.info("redirect_to_with_session: " + auxFullUrl);
			response.encodeRedirectURL(auxFullUrl);
			break;
		case KConstants.NextStep.none:
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
