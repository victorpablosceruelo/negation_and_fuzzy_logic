package urls;

import filesAndPaths.PathsUtils;

public class UrlsTools {

	private boolean isFirstParam = true;
	private StringBuilder currentUrl = null;

	public UrlsTools(boolean withSubPath, String url) {
		if (url == null)
			url = "";
		String path;
		if (withSubPath) {
			path = PathsUtils.concatPathsStrings(AppUrl.getAppUrl(), url);
		} else {
			path = url;
		}
		this.currentUrl = new StringBuilder();
		this.currentUrl.append(path);
		isFirstParam = true;
	}

	public void addParam(String paramName, String paramValue) {
		if ((paramName == null) || ("".equals(paramName)))
			return;
		if ((paramValue == null) || ("".equals(paramValue)))
			return;

		if (this.isFirstParam) {
			this.isFirstParam = false;
			this.currentUrl.append("?");
		} else {
			this.currentUrl.append("&");
		}
		this.currentUrl.append(paramName + "=" + paramValue);
	}

	public String getResult() {
		return this.currentUrl.toString();
	}
}
