package constants;

import urls.UrlMap;

public class KUrls {

	public static class Servlets {
		public static final String MainServlet = "MainServlet";
		public static final String AuthServlet = "AuthServlet";
	}
	public static class Pages {
		public static final UrlMap EmptyPage = new UrlMap("emptyPage", "", "", "");
		public static final UrlMap IndexPage = new UrlMap("indexPage", "", "", "index.jsp");
		public static final UrlMap ExceptionAjaxPage = new UrlMap("exceptionAjaxPage", "", "", "WEB-INF/exceptionAjaxPage.jsp");
		public static final UrlMap NullSessionAjaxPage = new UrlMap("nullSessionAjaxPage", "", "", "WEB-INF/nullSessionAjaxPage.jsp");
	}

	public static class Auth {
		public static String servlet = Servlets.AuthServlet;
		public static String manager = "";
		public static final UrlMap AuthServlet = new UrlMap(servlet, manager, "", "", "", "");
		public static final UrlMap SignIn = new UrlMap("signIn", "signin", AuthServlet.getKeyString(), "WEB-INF/signedIn.jsp");
		public static final UrlMap SignOut = new UrlMap("signOut", "signout", "SocialAuthCallBackServlet", "WEB-INF/signedOut.jsp");
		
		public static UrlMap [] opsList = {AuthServlet, SignIn, SignOut};
	}

	public static class User {
		public static String manager = "UserManager";
		public static final UrlMap UserOptions = new UrlMap("userOptions", "userInfo", "DispatcherServlet", "WEB-INF/userOptions.jsp");
	}

	public static class Files {
		public static String manager = "FilesManager";
		public static final UrlMap List = new UrlMap(manager, "list", "listProgramFiles", ,
				"WEB-INF/filesList.jsp");
		public static final UrlMap Upload = new UrlMap("fileUploadRequest", "fileUpload", "DispatcherServlet", "WEB-INF/fileUpload.jsp");
		public static final UrlMap View = new UrlMap("fileViewRequest", "fileView", "DispatcherServlet", "WEB-INF/fileView.jsp");
		public static final UrlMap Download = new UrlMap("fileDownloadRequest", "fileDownload", "DispatcherServlet",
				"WEB-INF/fileDownload.jsp");
		public static final UrlMap FileRemove = new UrlMap("fileRemoveRequest", "fileRemove", "DispatcherServlet", "WEB-INF/fileRemove.jsp");
	}

	public static class Queries {
		public static final UrlMap IntrospectionQuery = new UrlMap("introspectionQuery", "introspectionQuery", "DispatcherServlet",
				"WEB-INF/programFileIntrospection.jsp");
		public static final UrlMap RunQuery = new UrlMap("runQuery", "runQuery", "DispatcherServlet", "WEB-INF/runQuery.jsp");
	}

	public static class Edit {
		public static final UrlMap ListProgramFuzzifications = new UrlMap("listProgramFuzzifications", "listProgramFuzzifications",
				"DispatcherServlet", "WEB-INF/listFuzzifications.jsp");
		public static final UrlMap SaveProgramFuzzification = new UrlMap("saveProgramFuzzification", "saveProgramFuzzification",
				"DispatcherServlet", "WEB-INF/saveFuzzification.jsp");
	}

	public static final UrlMap[] pagesList = { EmptyPage, IndexPage, ExceptionAjaxPage, NullSessionAjaxPage, SocialAuthenticationCallBack,
			SignIn, SignOut, UserOptions, ListProgramFiles, FileUpload, FileView, FileDownload, IntrospectionQuery, RunQuery,
			ListProgramFuzzifications, SaveProgramFuzzification };

}
