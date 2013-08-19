package constants;

import urls.UrlMap;

public class KPages {

	public static final UrlMap EmptyPage = new UrlMap("emptyPage", "", "");
	public static final UrlMap IndexPage = new UrlMap("indexPage", "", "index.jsp");
	public static final UrlMap ExceptionAjaxPage = new UrlMap("exceptionAjaxPage", "", "WEB-INF/exceptionAjaxPage.jsp");
	public static final UrlMap NullSessionAjaxPage = new UrlMap("nullSessionAjaxPage", "", "WEB-INF/nullSessionAjaxPage.jsp");

	public static final UrlMap SocialAuthenticationCallBackRequest = new UrlMap("SocialAuthenticationCallBackRequest", "",
			"SocialAuthCallBackServlet");
	public static final UrlMap SignInRequest = new UrlMap("signInRequest", "signin", "SocialAuthCallBackServlet");
	public static final UrlMap SignOutRequest = new UrlMap("signOutRequest", "signout", "SocialAuthCallBackServlet");
	public static final UrlMap SignedInAnswer = new UrlMap("signedInAnswer", "", "WEB-INF/signedIn.jsp");
	public static final UrlMap SignedOutAnswer = new UrlMap("signedOutAnswer", "", "WEB-INF/signedOut.jsp");
	public static final UrlMap UserOptionsRequest = new UrlMap("userOptionsRequest", "userInfo", "DispatcherServlet");
	public static final UrlMap UserOptionsAnswer = new UrlMap("userOptionsAnswer", "", "WEB-INF/userOptions.jsp");

	public static final UrlMap FilesListRequest = new UrlMap("filesListRequest", "filesList", "DispatcherServlet");
	public static final UrlMap FilesListAnswer = new UrlMap("filesListAnswer", "", "WEB-INF/filesList.jsp");
	public static final UrlMap FileUploadRequest = new UrlMap("fileUploadRequest", "fileUpload", "DispatcherServlet");
	public static final UrlMap FileUploadAnswer = new UrlMap("fileUploadAnswer", "", "WEB-INF/fileUpload.jsp");
	public static final UrlMap FileViewRequest = new UrlMap("fileViewRequest", "fileView", "DispatcherServlet");
	public static final UrlMap FileViewAnswer = new UrlMap("fileViewAnswer", "", "WEB-INF/fileView.jsp");
	public static final UrlMap FileDownloadRequest = new UrlMap("fileDownloadRequest", "fileDownload", "DispatcherServlet");
	public static final UrlMap FileDownloadAnswer = new UrlMap("fileDownloadAnswer", "", "WEB-INF/fileDownload.jsp");
	public static final UrlMap FileRemoveRequest = new UrlMap("fileRemoveRequest", "fileRemove", "DispatcherServlet");
	public static final UrlMap FileRemoveAnswer = new UrlMap("fileRemoveAnswer", "", "WEB-INF/fileRemove.jsp");

	public static final UrlMap introspectionQueryRequest = new UrlMap("introspectionQueryRequest", "programFileIntrospection",
			"DispatcherServlet");
	public static final UrlMap introspectionQueryAnswer = new UrlMap("introspectionQueryAnswer", "",
			"WEB-INF/programFileIntrospection.jsp");
	public static final UrlMap RunQueryRequest = new UrlMap("runQueryRequest", "runQuery", "DispatcherServlet");
	public static final UrlMap RunQueryAnswer = new UrlMap("runQueryAnswer", "", "WEB-INF/runQuery.jsp");

	public static final UrlMap ListProgramFuzzificationsRequest = new UrlMap("listProgramFuzzificationsRequest",
			"listProgramFuzzifications", "DispatcherServlet");
	public static final UrlMap ListProgramFuzzificationsAnswer = new UrlMap("listProgramFuzzificationsAnswer", "",
			"WEB-INF/listFuzzifications.jsp");
	public static final UrlMap SaveProgramFuzzificationRequest = new UrlMap("saveProgramFuzzificationRequest",
			"saveProgramFuzzification", "DispatcherServlet");
	public static final UrlMap SaveProgramFuzzificationAnswer = new UrlMap("saveProgramFuzzificationAnswer", "",
			"WEB-INF/saveFuzzification.jsp");

	public static final UrlMap[] pagesList = { EmptyPage, IndexPage, ExceptionAjaxPage, NullSessionAjaxPage,
			SocialAuthenticationCallBackRequest, SignInRequest, SignOutRequest, SignedInAnswer, SignedOutAnswer, UserOptionsRequest,
			FilesListRequest, FilesListAnswer, FileUploadRequest, FileUploadAnswer, FileViewRequest, FileViewAnswer,
			FileDownloadRequest, FileDownloadAnswer, introspectionQueryRequest, introspectionQueryAnswer, RunQueryRequest,
			RunQueryAnswer, ListProgramFuzzificationsRequest, ListProgramFuzzificationsAnswer, SaveProgramFuzzificationRequest,
			SaveProgramFuzzificationAnswer };

}
