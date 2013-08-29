package constants;

import urls.UrlMap;

public class KUrls {

	public static class Pages {
		public static final UrlMap Empty = new UrlMap("", "emptyPage", null, null, "");
		public static final UrlMap Index = new UrlMap("", "indexPage", null, null, "index.jsp");
		public static final UrlMap Exception = new UrlMap("", "exception", null, null, "WEB-INF/exceptionAjaxPage.jsp");
		public static final UrlMap NullSession = new UrlMap("", "nullSession", null, null, "WEB-INF/nullSessionAjaxPage.jsp");

		public static UrlMap[] list = { Empty, Index, Exception, NullSession };
	}

	public static class Auth {
		public static String manager = "AuthManager";
		public static final UrlMap SocialAuthCallback = new UrlMap("", "", null, null, "");

		public static final UrlMap SignInPage = new UrlMap(manager, "signInPage", null, null, "WEB-INF/signedIn.jsp");
		public static final UrlMap SignIn = new UrlMap(manager, "signIn", SignInPage, KUrls.Pages.Exception, "");

		public static final UrlMap SignOutPage = new UrlMap(manager, "signOutPage", null, null, "WEB-INF/signedOut.jsp");
		public static final UrlMap SignOut = new UrlMap(manager, "signOut", SignOutPage, KUrls.Pages.Exception, "");

		public static UrlMap[] list = { SocialAuthCallback, SignInPage, SignIn, SignOutPage, SignOut };
	}

	public static class User {
		public static String manager = "UserManager";

		public static final UrlMap UserOptionsPage = new UrlMap(manager, "userOptionsPage", null, null, "WEB-INF/userOptions.jsp");
		public static final UrlMap UserOptions = new UrlMap(manager, "userOptions", UserOptionsPage, KUrls.Pages.Exception, "");

		public static UrlMap[] list = { UserOptionsPage, UserOptions };
	}

	public static class Files {
		public static String manager = "FilesManager";
		public static final UrlMap ListPage = new UrlMap(manager, "listPage", null, null, "WEB-INF/files/list.jsp");
		public static final UrlMap List = new UrlMap(manager, "list", ListPage, KUrls.Pages.Exception, "");

		public static final UrlMap UploadPage = new UrlMap(manager, "uploadPage", null, null, "WEB-INF/files/upload.jsp");
		public static final UrlMap Upload = new UrlMap(manager, "upload", UploadPage, KUrls.Pages.Exception, "");

		public static final UrlMap ViewPage = new UrlMap(manager, "viewPage", null, null, "WEB-INF/files/view.jsp");
		public static final UrlMap View = new UrlMap(manager, "view", ViewPage, KUrls.Pages.Exception, "");

		public static final UrlMap DownloadPage = new UrlMap(manager, "downloadPage", null, null, "WEB-INF/files/download.jsp");
		public static final UrlMap Download = new UrlMap(manager, "download", DownloadPage, KUrls.Pages.Exception, "");

		public static final UrlMap RemovePage = new UrlMap(manager, "removePage", null, null, "WEB-INF/files/remove.jsp");
		public static final UrlMap Remove = new UrlMap(manager, "remove", RemovePage, KUrls.Pages.Exception, "");

		public static UrlMap[] list = { ListPage, List, UploadPage, Upload, ViewPage, View, DownloadPage, Download, RemovePage, Remove };
	}

	public static class Queries {
		public static String manager = "QueriesManager";

		public static final UrlMap IntrospectionPage = new UrlMap(manager, "", null, null, "WEB-INF/queries/introspection.jsp");
		public static final UrlMap Introspection = new UrlMap(manager, "introspection", IntrospectionPage, KUrls.Pages.Exception, "");

		public static final UrlMap EvaluatePage = new UrlMap(manager, "", null, null, "WEB-INF/runQuery.jsp");
		public static final UrlMap Evaluate = new UrlMap(manager, "evaluate", EvaluatePage, KUrls.Pages.Exception, "");

		public static UrlMap[] list = { IntrospectionPage, Introspection, EvaluatePage, Evaluate };
	}

	public static class Fuzzifications {
		public static String manager = "FuzzificationsManager";

		public static final UrlMap ListPage = new UrlMap(manager, "", null, null, "WEB-INF/listFuzzifications.jsp");
		public static final UrlMap List = new UrlMap(manager, "list", ListPage, KUrls.Pages.Exception, "");

		public static final UrlMap SavePage = new UrlMap(manager, "", null, null, "WEB-INF/saveFuzzification.jsp");
		public static final UrlMap Save = new UrlMap(manager, "save", SavePage, KUrls.Pages.Exception, "");

		public static UrlMap[] list = { ListPage, List, SavePage, Save };
	}

	public static final UrlMap[] list() {
		int size = Pages.list.length + Auth.list.length + User.list.length + Files.list.length + Queries.list.length
				+ Fuzzifications.list.length;
		UrlMap[] tmpList = new UrlMap[size];
		return tmpList;
	}

}
