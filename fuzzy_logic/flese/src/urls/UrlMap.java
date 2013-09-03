package urls;

import storeHouse.RequestStoreHouse;
import constants.KConstants;

public class UrlMap {
	
	private String manager = null;
	private String op = null;
	private UrlMap nextPage = null;
	private UrlMap exceptionPage = null;
	private String currentUrl = null;

	protected UrlMap(String manager, String op, UrlMap nextPage, UrlMap exceptionPage) {
		initializeUrlMap(manager, op, nextPage, exceptionPage, "");
	}

	public UrlMap(String manager, String op, UrlMap nextPage, UrlMap exceptionPage, String currentUrl) {
		initializeUrlMap(manager, op, nextPage, exceptionPage, currentUrl);
	}
	
	public UrlMap(RequestStoreHouse sessionStoreHouse) {
		String manager = sessionStoreHouse.getRequestParameter(KConstants.Request.managerParam);
		String op = sessionStoreHouse.getRequestParameter(KConstants.Request.operationParam);
		initializeUrlMap(manager, op, null, null, null);
	}

	private void initializeUrlMap(String manager, String op, UrlMap nextPage, UrlMap exceptionPage, String currentUrl) {
		if (manager == null)
			manager = "";
		if (op == null)
			op = "";		
		if (currentUrl == null)
			currentUrl = "";

		this.manager = manager;
		this.op = op;
		this.nextPage = nextPage;
		this.exceptionPage = exceptionPage;
		this.currentUrl = currentUrl;
	}

	public String getManager() {
		return this.manager;
	};

	public String getOp() {
		return this.op;
	};

	public String getUrl(boolean withSubPath, boolean isAjax) {
				
		UrlsTools urlTool = null;
		
		if ((this.currentUrl != null) && (! "".equals(this.currentUrl))) {
			urlTool = new UrlsTools(withSubPath, this.currentUrl);
		}
		else {
			urlTool = new UrlsTools(withSubPath, KConstants.servletName);
		}

		urlTool.addParam(KConstants.Request.managerParam, removeManagerTail(this.manager));
		urlTool.addParam(KConstants.Request.operationParam, this.op);
		urlTool.addParam(KConstants.Request.isAjaxParam, isAjax ? KConstants.Values.True : KConstants.Values.False);
		
		return urlTool.getResult();
	};
	
	private String removeManagerTail(String managerName) {
		if ((managerName != null) && (!"".equals(managerName))) {
			if (managerName.endsWith(KConstants.Managers.managerSuffix)) {
				int end = managerName.length() - KConstants.Managers.managerSuffix.length();
				managerName = managerName.substring(0, end);
			}
		}
		return managerName;
	}

	public String getCurrentUrl() {
		return this.currentUrl;
	}

	public String getNextPage() {
		if (this.nextPage != null)
			return this.nextPage.getCurrentUrl();
		return "";
	}

	public String getExceptionPage() {
		if (this.exceptionPage != null)
			return this.exceptionPage.getCurrentUrl();
		return "";
	}
}
