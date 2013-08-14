package constants;

import urls.UrlMap;

public class KConstants {

	public static class Pages {
		public static final UrlMap TheSamePage = new UrlMap("theSamePage", "", "");
		public static final UrlMap IndexPage = new UrlMap("IndexPage", "", "index.jsp");
		public static final UrlMap ExceptionAjaxPage = new UrlMap("ExceptionAjaxPage", "", "WEB-INF/exceptionAjaxPage.jsp");
		public static final UrlMap NullSessionAjaxPage = new UrlMap("NullSessionAjaxPage", "", "WEB-INF/nullSessionAjaxPage.jsp");

		public static final UrlMap SocialAuthenticationCallBackRequest = new UrlMap("SocialAuthenticationCallBackRequest", "",
				"SocialAuthCallBackServlet");
		public static final UrlMap SignInRequest = new UrlMap("SignInRequest", "signin", "SocialAuthCallBackServlet");
		public static final UrlMap SignOutRequest = new UrlMap("SignOutRequest", "signout", "SocialAuthCallBackServlet");
		public static final UrlMap SignedInAnswer = new UrlMap("SignedInAnswer", "", "WEB-INF/signedIn.jsp");
		public static final UrlMap SignedOutAnswer = new UrlMap("SignedOutAnswer", "", "WEB-INF/signedOut.jsp");
		public static final UrlMap UserOptionsRequest = new UrlMap("UserOptionsRequest", "userInfo", "DispatcherServlet");
		public static final UrlMap UserOptionsAnswer = new UrlMap("UserOptionsAnswer", "", "WEB-INF/userOptions.jsp");

		public static final UrlMap FilesListRequest = new UrlMap("FilesListRequest", "filesList", "DispatcherServlet");
		public static final UrlMap FilesListAnswer = new UrlMap("FilesListAnswer", "", "WEB-INF/filesList.jsp");
		public static final UrlMap FileUploadRequest = new UrlMap("FileUploadRequest", "fileUpload", "DispatcherServlet");
		public static final UrlMap FileUploadAnswer = new UrlMap("FileUploadAnswer", "", "WEB-INF/fileUpload.jsp");
		public static final UrlMap FileViewRequest = new UrlMap("FileViewRequest", "fileView", "DispatcherServlet");
		public static final UrlMap FileViewAnswer = new UrlMap("FileViewAnswer", "", "WEB-INF/fileView.jsp");
		public static final UrlMap FileDownloadRequest = new UrlMap("FileDownloadRequest", "fileDownload", "DispatcherServlet");
		public static final UrlMap FileDownloadAnswer = new UrlMap("FileDownloadAnswer", "", "WEB-INF/fileDownload.jsp");
		public static final UrlMap FileRemoveRequest = new UrlMap("FileRemoveRequest", "fileRemove", "DispatcherServlet");
		public static final UrlMap FileRemoveAnswer = new UrlMap("FileRemoveAnswer", "", "WEB-INF/fileRemove.jsp");

		public static final UrlMap ProgramFileIntrospectionRequest = new UrlMap("ProgramFileIntrospectionRequest",
				"programFileIntrospection", "DispatcherServlet");
		public static final UrlMap ProgramFileIntrospectionAnswer = new UrlMap("ProgramFileIntrospectionAnswer", "",
				"WEB-INF/programFileIntrospection.jsp");
		public static final UrlMap RunQueryRequest = new UrlMap("RunQueryRequest", "runQuery", "DispatcherServlet");
		public static final UrlMap RunQueryAnswer = new UrlMap("RunQueryAnswer", "", "WEB-INF/runQuery.jsp");

		public static final UrlMap ListProgramFuzzificationsRequest = new UrlMap("ListProgramFuzzificationsRequest",
				"listProgramFuzzifications", "DispatcherServlet");
		public static final UrlMap ListProgramFuzzificationsAnswer = new UrlMap("ListProgramFuzzificationsAnswer", "",
				"WEB-INF/listFuzzifications.jsp");
		public static final UrlMap SaveProgramFuzzificationRequest = new UrlMap("SaveProgramFuzzificationRequest",
				"saveProgramFuzzification", "DispatcherServlet");
		public static final UrlMap SaveProgramFuzzificationAnswer = new UrlMap("SaveProgramFuzzificationAnswer", "",
				"WEB-INF/saveFuzzification.jsp");

		public static final UrlMap[] pagesList = { TheSamePage, IndexPage, ExceptionAjaxPage, NullSessionAjaxPage,
				SocialAuthenticationCallBackRequest, SignInRequest, SignOutRequest, SignedInAnswer, SignedOutAnswer, UserOptionsRequest,
				FilesListRequest, FilesListAnswer, FileUploadRequest, FileUploadAnswer, FileViewRequest, FileViewAnswer,
				FileDownloadRequest, FileDownloadAnswer, ProgramFileIntrospectionRequest, ProgramFileIntrospectionAnswer, RunQueryRequest,
				RunQueryAnswer, ListProgramFuzzificationsRequest, ListProgramFuzzificationsAnswer, SaveProgramFuzzificationRequest,
				SaveProgramFuzzificationAnswer };
	}
	
	public static String appPath = "flese/";
}
