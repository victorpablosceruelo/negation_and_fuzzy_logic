package urls;

import javax.servlet.http.HttpServletRequest;

import org.apache.commons.logging.Log;

public class UrlMap {

	private String keyString = null;
	private String opValue = null;
	private String incompleteUrl = null;
	private String method = null;

	public UrlMap(String keyString, String opValue, String incompleteUrl, String method) {

		if (keyString == null)
			keyString = "";
		if (opValue == null)
			opValue = "";
		if (incompleteUrl == null)
			incompleteUrl = "";

		this.keyString = keyString;
		this.opValue = opValue;
		this.incompleteUrl = incompleteUrl;
		this.method = method;
	}

	public String getKeyString() {
		return this.keyString;
	};

	public String getOpValue() {
		return this.opValue;
	};

	public String getUrl() {
		String url = this.incompleteUrl;
		if (!"".equals(this.opValue)) {
			url += "?op=" + this.opValue;
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
	
	public String getMethod() {
		return this.method;
	}
	
}
