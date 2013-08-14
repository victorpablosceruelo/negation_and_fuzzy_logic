package urls;

import javax.servlet.http.HttpServletRequest;

import org.apache.commons.logging.Log;

public class UrlMap {

	private String keyString = null;
	private String opValue = null;
	private String incompleteUrl = null;

	public UrlMap(String keyString, String opValue, String incompleteUrl) {

		if (keyString == null)
			keyString = "";
		if (opValue == null)
			opValue = "";
		if (incompleteUrl == null)
			incompleteUrl = "";

		this.keyString = keyString;
		this.opValue = opValue;
		this.incompleteUrl = incompleteUrl;
	}

	public String getKeyString() {
		return keyString;
	};

	public String getOpValue() {
		return opValue;
	};
	
	public String getUrl() {
		String url = incompleteUrl;
		if (!"".equals(opValue)) {
			url += "?op=" + opValue;
		}
		return url;
	};

	public String getFullUrl(HttpServletRequest request, Log LOG) {
		String appUrl = AppUrl.getAppUrl(request, LOG);
		String url = "";
		if (!"".equals(getUrl()))
			url = appUrl + getUrl();
		if (LOG != null)
			LOG.info("url: " + url);
		return url;

	}
}
