package urls;

import storeHouse.RequestStoreHouse;
import constants.KConstants;

public class UrlMap {

	private String manager = null;
	private String op = null;
	private UrlMap nextPage = null;
	private UrlMap exceptionPage = null;
	private String currentUrl = null;

	private class Constants {
		public static final String defaultManager = "defaultManager";
		public static final String defaultOp = "defaultOp";
	}

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

		this.manager = adequateManagerName(manager);
		this.op = adequateOpName(op);
		this.nextPage = nextPage;
		this.exceptionPage = exceptionPage;
		this.currentUrl = currentUrl;
	}

	private String adequateManagerName(String managerName) {
		if ((managerName == null) || ("".equals(managerName))) {
			return Constants.defaultManager;
		}
		if (!managerName.endsWith(KConstants.Managers.managerSuffix)) {
			return managerName + KConstants.Managers.managerSuffix;
		}
		return managerName;
	}

	private String adequateOpName(String op) {
		if ((op == null) || ("".equals(op))) {
			return Constants.defaultOp;
		}
		return op;
	}

	public String getManager() {
		return this.manager;
	};

	public String getOp() {
		return this.op;
	};

	public String getUrl(boolean isAjax) {
		return getUrl(true, isAjax);
	}

	public String getUrl(boolean prependSubPath, boolean isAjax) {

		String url = null;

		// If there is no current url, take the main servlet as the url.
		if ((this.currentUrl == null) || ("".equals(this.currentUrl))) {
			url = KConstants.servletName;
		} else {
			url = this.currentUrl;
		}
		UrlsTools urlTool = new UrlsTools(prependSubPath, url);

		urlTool.addParam(KConstants.Request.managerParam, managerNameToParam(this.manager));
		urlTool.addParam(KConstants.Request.operationParam, operationNameToParam(this.op));
		urlTool.addParam(KConstants.Request.isAjaxParam, isAjax ? KConstants.Values.True : KConstants.Values.False);

		return urlTool.getResult();
	};

	private String managerNameToParam(String managerName) {
		if ((managerName != null) && (!"".equals(managerName))) {
			if (Constants.defaultManager.equals(managerName)) {
				managerName = "";
			} else {
				if (managerName.endsWith(KConstants.Managers.managerSuffix)) {
					int end = managerName.length() - KConstants.Managers.managerSuffix.length();
					managerName = managerName.substring(0, end);
				}
			}
		}
		return managerName;
	}

	private String operationNameToParam(String opName) {
		if ((opName != null) && (!"".equals(opName))) {
			if (Constants.defaultManager.equals(opName)) {
				opName = "";
			}
		}
		return opName;
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
