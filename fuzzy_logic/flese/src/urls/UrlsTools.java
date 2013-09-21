package urls;

import filesAndPaths.PathsUtils;

public class UrlsTools {

	private boolean hasParams;
	private StringBuilder url;

	public UrlsTools(boolean withSubPath, String url) {
		if (url == null)
			url = "";
		this.url = new StringBuilder();
		String path;
		if (withSubPath) {
			if (!url.startsWith(AppUrl.getAppPath())) {
				path = PathsUtils.concatPathsStrings(AppUrl.getAppPath(), url);
			} else {
				path = url;
			}
		} else {
			path = url;
		}
		this.url.append(path);
		hasParams = false;
	}

	public void addParam(String paramName, String paramValue) {
		if ((paramName == null) || ("".equals(paramName)))
			return;
		if ((paramValue == null) || ("".equals(paramValue)))
			return;

		if (!this.hasParams) {
			this.hasParams = true;
			this.url.append("?");
		} else {
			this.url.append("&");
		}
		this.url.append(paramName + "=" + paramValue);
	}

	public String getResult() {
		return this.url.toString();
	}
}
