package urls;

public class UrlMap {

	private String manager = null;
	private String op = null;
	private UrlMap nextPage = null;
	private UrlMap exceptionPage = null;
	private String currentUrl = null;

	protected UrlMap(String manager, String op, UrlMap nextPage, UrlMap exceptionPage) throws UrlMapException {
		initializeUrlMap(manager, op, nextPage, exceptionPage, "");
	}

	public UrlMap(String manager, String op, UrlMap nextPage, UrlMap exceptionPage, String currentUrl) throws UrlMapException {
		initializeUrlMap(manager, op, nextPage, exceptionPage, currentUrl);
	}

	private void initializeUrlMap(String manager, String op, UrlMap nextPage, UrlMap exceptionPage, String currentUrl) throws UrlMapException {
		if (manager == null)
			throw new UrlMapException("manager cannot be null");
		if (op == null)
			throw new UrlMapException("op cannot be null");

		if ("".equals(manager))
			throw new UrlMapException("manager cannot be empty string");
		if ("".equals(op))
			throw new UrlMapException("op cannot be empty string");
		
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

	public String getUrl(boolean isAjax) {
		String url = null;
		String opParam = null;
		String isAjaxParam = null;

		if (this.manager == "")
			url = this.currentUrl;
		else {
			url = this.manager;
		}

		if (!"".equals(this.op)) {
			opParam = "op=" + this.op;
		}

		if (isAjax) {
			isAjaxParam = "ajax=true";
		}
		
		if ((opParam != null) && (isAjaxParam != null)) {
			return url + "?" + opParam + "&" + isAjaxParam;
		}
		else {
			if (opParam != null) {
				return url + "?" + opParam;
			}
			if (isAjaxParam != null) {
				return url + "?" + isAjaxParam;
			}
		}
		return url;
	};

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
