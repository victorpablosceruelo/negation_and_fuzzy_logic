package constants;

import storeHouse.StoreHouseClass;


public class UrlMappingsClass {

	public static class Pages {
		public static final int TheSamePage = 0;
		public static final int IndexPage = 1;
		public static final int ExceptionAjaxPage = 2;
		public static final int NullSessionAjaxPage = 3;

		public static final int SocialAuthenticationCallBackRequest = 4;
		public static final int SignInRequest = 5;
		public static final int SignOutRequest = 6;
		public static final int SignedInAnswer = 7;
		public static final int SignedOutAnswer = 8;
		public static final int UserOptionsRequest = 9;
		public static final int UserOptionsAnswer = 10;

		public static final int FilesListRequest = 11;
		public static final int FilesListAnswer = 12;
		public static final int FileUploadRequest = 13;
		public static final int FileUploadAnswer = 14;
		public static final int FileViewRequest = 15;
		public static final int FileViewAnswer = 16;
		public static final int FileDownloadRequest = 17;
		public static final int FileDownloadAnswer = 18;
		public static final int FileRemoveRequest = 19;
		public static final int FileRemoveAnswer = 20;

		public static final int ProgramFileIntrospectionRequest = 21;
		public static final int ProgramFileIntrospectionAnswer = 22;
		public static final int RunQueryRequest = 23;
		public static final int RunQueryAnswer = 24;

		public static final int ListProgramFuzzificationsRequest = 25;
		public static final int ListProgramFuzzificationsAnswer = 26;
		public static final int SaveProgramFuzzificationRequest = 27;
		public static final int SaveProgramFuzzificationAnswer = 28;
	}
	
	private static boolean loaded = false;
	
	public UrlMappingsClass() { 	
	}
	
	private static void storeMapping(UrlMappingClass urlMapping) throws Exception {
		int key = urlMapping.getKey();
		StoreHouseClass.store(UrlMappingsClass.class.getName(), key, urlMapping);
	}
	
	public static UrlMappingClass getMapping(int key) throws Exception {
		if (! loaded) {
			load();
		}
			return (UrlMappingClass) StoreHouseClass.retrieve(UrlMappingsClass.class.getName(), key);
	}
	
	private static void load() throws Exception {
		
		storeMapping(new UrlMappingClass(Pages.TheSamePage, "theSamePage", "", ""));
		storeMapping(new UrlMappingClass(Pages.IndexPage, "IndexPage", "", "index.jsp"));
		storeMapping(new UrlMappingClass(Pages.ExceptionAjaxPage, "ExceptionAjaxPage", "", "WEB-INF/exceptionAjaxPage.jsp"));
		storeMapping(new UrlMappingClass(Pages.NullSessionAjaxPage, "NullSessionAjaxPage", "", "WEB-INF/nullSessionAjaxPage.jsp"));
		
		storeMapping(new UrlMappingClass(Pages.SocialAuthenticationCallBackRequest, "SocialAuthenticationCallBackRequest", "", "SocialAuthCallBackServlet"));
		storeMapping(new UrlMappingClass(Pages.SignInRequest, "SignInRequest", "signin", "SocialAuthCallBackServlet"));
		storeMapping(new UrlMappingClass(Pages.SignOutRequest, "SignOutRequest", "signout", "SocialAuthCallBackServlet"));
		storeMapping(new UrlMappingClass(Pages.SignedInAnswer, "SignedInAnswer", "", "WEB-INF/signedIn.jsp"));
		storeMapping(new UrlMappingClass(Pages.SignedOutAnswer, "SignedOutAnswer", "", "WEB-INF/signedOut.jsp"));
		storeMapping(new UrlMappingClass(Pages.UserOptionsRequest, "UserOptionsRequest", "userInfo", "DispatcherServlet"));
		storeMapping(new UrlMappingClass(Pages.UserOptionsAnswer, "UserOptionsAnswer", "", "WEB-INF/userOptions.jsp"));
		
		storeMapping(new UrlMappingClass(Pages.FilesListRequest, "FilesListRequest", "filesList", "DispatcherServlet"));
		storeMapping(new UrlMappingClass(Pages.FilesListAnswer, "FilesListAnswer", "", "WEB-INF/filesList.jsp"));
		storeMapping(new UrlMappingClass(Pages.FileUploadRequest, "FileUploadRequest", "fileUpload", "DispatcherServlet"));
		storeMapping(new UrlMappingClass(Pages.FileUploadAnswer, "FileUploadAnswer", "", "WEB-INF/fileUpload.jsp"));
		storeMapping(new UrlMappingClass(Pages.FileViewRequest, "FileViewRequest", "fileView", "DispatcherServlet"));
		storeMapping(new UrlMappingClass(Pages.FileViewAnswer, "FileViewAnswer", "", "WEB-INF/fileView.jsp"));
		storeMapping(new UrlMappingClass(Pages.FileDownloadRequest, "FileDownloadRequest", "fileDownload", "DispatcherServlet"));
		storeMapping(new UrlMappingClass(Pages.FileDownloadAnswer, "FileDownloadAnswer", "", "WEB-INF/fileDownload.jsp"));
		storeMapping(new UrlMappingClass(Pages.FileRemoveRequest, "FileRemoveRequest", "fileRemove", "DispatcherServlet"));
		storeMapping(new UrlMappingClass(Pages.FileRemoveAnswer, "FileRemoveAnswer", "", "WEB-INF/fileRemove.jsp"));
		
		storeMapping(new UrlMappingClass(Pages.ProgramFileIntrospectionRequest, "ProgramFileIntrospectionRequest", "programFileIntrospection", "DispatcherServlet"));
		storeMapping(new UrlMappingClass(Pages.ProgramFileIntrospectionAnswer, "ProgramFileIntrospectionAnswer", "", "WEB-INF/programFileIntrospection.jsp"));
		storeMapping(new UrlMappingClass(Pages.RunQueryRequest, "RunQueryRequest", "runQuery", "DispatcherServlet"));
		storeMapping(new UrlMappingClass(Pages.RunQueryAnswer, "RunQueryAnswer", "", "WEB-INF/runQuery.jsp"));
		
		storeMapping(new UrlMappingClass(Pages.ListProgramFuzzificationsRequest, "ListProgramFuzzificationsRequest", "listProgramFuzzifications", "DispatcherServlet"));
		storeMapping(new UrlMappingClass(Pages.ListProgramFuzzificationsAnswer, "ListProgramFuzzificationsAnswer", "", "WEB-INF/listFuzzifications.jsp"));
		storeMapping(new UrlMappingClass(Pages.SaveProgramFuzzificationRequest, "SaveProgramFuzzificationRequest", "saveProgramFuzzification", "DispatcherServlet"));
		storeMapping(new UrlMappingClass(Pages.SaveProgramFuzzificationAnswer, "SaveProgramFuzzificationAnswer", "", "WEB-INF/saveFuzzification.jsp"));
	}
}





/* --- */
