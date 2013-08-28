package urls;

public class UrlMap {

	private String keyString = null;
	private String opValue = null;
	private String incompleteUrl = null;
	private String nextUrl = null;

	public UrlMap(String keyString, String opValue, String incompleteUrl, String nextUrl) {

		if (keyString == null)
			keyString = "";
		if (opValue == null)
			opValue = "";
		if (incompleteUrl == null)
			incompleteUrl = "";
		if (nextUrl == null)
			nextUrl = "";

		this.keyString = keyString;
		this.opValue = opValue;
		this.incompleteUrl = incompleteUrl;
		this.nextUrl = nextUrl;
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

	public String getFullUrl(String requestUrl, String serverName) {
		String appUrl = AppUrl.getAppUrl(requestUrl, serverName);
		String url = appUrl;
		if (!"".equals(getUrl()))
			url = appUrl + getUrl();
		return url;
	}

}
