package urls;

import constants.KConstants;
import filesAndPaths.PathsUtils;

public class UrlsTools {

	private boolean hasParams;
	private StringBuilder url;
	
	public UrlsTools(String url) {
		this.url = new StringBuilder();
		this.url.append(PathsUtils.concatPathsStrings(KConstants.appPath, url));
		hasParams = false;
	}
	
	public void addParam(String paramName, String paramValue) {
		if ((paramValue == null) || ("".equals(paramValue))) return;
		
		if (! this.hasParams) {
			this.hasParams = true;
			this.url.append("?");
		}
		else {
			this.url.append("&");		
		}
		this.url.append(paramName + "=" + paramValue);
	}
	
	public String getResult() {
		return this.url.toString();
	}
}
