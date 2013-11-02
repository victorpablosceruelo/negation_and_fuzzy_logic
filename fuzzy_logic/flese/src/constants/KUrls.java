package constants;

import urls.UrlMap;

public class KUrls {

	public static class Pages {
		public static final UrlMap Empty = new UrlMap("", "emptyPage", null, null, "");
		public static final UrlMap Index = new UrlMap("", "indexPage", null, null, "index.jsp");
		public static final UrlMap Exception = new UrlMap("", "exception", null, null, "WEB-INF/errorException.jsp");
		public static final UrlMap NullSession = new UrlMap("", "nullSession", null, null, "WEB-INF/errorNullSession.jsp");
	}

	public static class Auth {
		public static String manager = "AuthManager";
		public static final UrlMap SocialAuthCallback = new UrlMap("", "", null, null, ""); // No manager nor op.
		
		public static final UrlMap ProvidersPage = new UrlMap(manager, "providersPage", null, null, "WEB-INF/authentication/providers.jsp");
		public static final UrlMap Providers = new UrlMap(manager, "providers", ProvidersPage, KUrls.Pages.Exception, "");

		public static final UrlMap SignInPage = new UrlMap(manager, "signInPage", null, null, "WEB-INF/authentication/signedIn.jsp");
		public static final UrlMap SignIn = new UrlMap(manager, "signIn", SignInPage, KUrls.Pages.Exception, "");

		public static final UrlMap SignOutPage = new UrlMap(manager, "signOutPage", null, null, "WEB-INF/authentication/signedOut.jsp");
		public static final UrlMap SignOut = new UrlMap(manager, "signOut", SignOutPage, KUrls.Pages.Exception, "");

		public static final UrlMap AboutPage = new UrlMap(manager, "aboutPage", null, null, "WEB-INF/authentication/about.jsp");
		public static final UrlMap About = new UrlMap(manager, "about", AboutPage, KUrls.Pages.Exception, "");

	}

	public static class User {
		public static String manager = "UserManager";

		public static final UrlMap OptionsPage = new UrlMap(manager, "optionsPage", null, null, "WEB-INF/user/userOptions.jsp");
		public static final UrlMap Options = new UrlMap(manager, "options", OptionsPage, KUrls.Pages.Exception, "");
	}

	public static class Files {
		public static String manager = "FilesManager";
		public static final UrlMap ListMyFilesPage = new UrlMap(manager, "listMyFilesPage", null, null, "WEB-INF/files/list.jsp");
		public static final UrlMap ListMyFiles = new UrlMap(manager, "listMyFiles", ListMyFilesPage, KUrls.Pages.Exception, "");

		public static final UrlMap ListPage = new UrlMap(manager, "listPage", null, null, "WEB-INF/files/list.jsp");
		public static final UrlMap List = new UrlMap(manager, "list", ListPage, KUrls.Pages.Exception, "");

		public static final UrlMap UploadDivPage = new UrlMap(manager, "uploadDivPage", null, null, "WEB-INF/files/uploadDiv.jsp");
		public static final UrlMap UploadDiv = new UrlMap(manager, "uploadDiv", UploadDivPage, KUrls.Pages.Exception, "");

		public static final UrlMap UploadPage = new UrlMap(manager, "uploadPage", null, null, "WEB-INF/files/upload.jsp");
		public static final UrlMap Upload = new UrlMap(manager, "upload", UploadPage, UploadPage, "");

		public static final UrlMap ViewPage = new UrlMap(manager, "viewPage", null, null, "WEB-INF/files/view.jsp");
		public static final UrlMap View = new UrlMap(manager, "view", ViewPage, KUrls.Pages.Exception, "");

		public static final UrlMap DownloadPage = new UrlMap(manager, "downloadPage", null, null, "WEB-INF/files/download.jsp");
		public static final UrlMap Download = new UrlMap(manager, "download", DownloadPage, KUrls.Pages.Exception, "");

		public static final UrlMap RemovePage = new UrlMap(manager, "removePage", null, null, "WEB-INF/files/remove.jsp");
		public static final UrlMap Remove = new UrlMap(manager, "remove", RemovePage, KUrls.Pages.Exception, "");
	}

	public static class Queries {
		public static String manager = "QueriesManager";

		public static final UrlMap SelectProgramFilePage = new UrlMap(manager, "", null, null, "WEB-INF/queries/selectProgramFile.jsp");
		public static final UrlMap SelectProgramFile = new UrlMap(manager, "selectProgramFile", SelectProgramFilePage, KUrls.Pages.Exception, "");

		public static final UrlMap ProgramFileActionsPage = new UrlMap(manager, "", null, null, "WEB-INF/queries/programFileActions.jsp");
		public static final UrlMap ProgramFileActions = new UrlMap(manager, "programFileActions", ProgramFileActionsPage, KUrls.Pages.Exception, "");

		public static final UrlMap SelectQueryStartTypePage = new UrlMap(manager, "", null, null, "WEB-INF/queries/selectQueryStartType.jsp");
		public static final UrlMap SelectQueryStartType = new UrlMap(manager, "selectQueryStartType", SelectQueryStartTypePage, KUrls.Pages.Exception, "");

		public static final UrlMap SelectQueryPage = new UrlMap(manager, "", null, null, "WEB-INF/queries/selectQuery.jsp");
		public static final UrlMap SelectQuery = new UrlMap(manager, "selectQuery", SelectQueryPage, KUrls.Pages.Exception, "");

		public static final UrlMap SelectQueryAddLinePage = new UrlMap(manager, "", null, null, "WEB-INF/queries/selectQueryAddLine.jsp");
		public static final UrlMap SelectQueryAddLine = new UrlMap(manager, "selectQueryAddLine", SelectQueryAddLinePage, KUrls.Pages.Exception, "");

		public static final UrlMap SelectQueryAddAggrPage = new UrlMap(manager, "", null, null, "WEB-INF/queries/selectQueryAddAggr.jsp");
		public static final UrlMap SelectQueryAddAggr = new UrlMap(manager, "selectQueryAddAggr", SelectQueryAddAggrPage, KUrls.Pages.Exception, "");

		public static final UrlMap SelectNegationPage = new UrlMap(manager, "", null, null, "WEB-INF/queries/selectNegation.jsp");
		public static final UrlMap SelectNegation = new UrlMap(manager, "selectNegation", SelectNegationPage, KUrls.Pages.Exception, "");

		public static final UrlMap SelectQuantifierPage = new UrlMap(manager, "", null, null, "WEB-INF/queries/selectQuantifier.jsp");
		public static final UrlMap SelectQuantifier = new UrlMap(manager, "selectQuantifier", SelectQuantifierPage, KUrls.Pages.Exception, "");

		public static final UrlMap SelectOperatorPage = new UrlMap(manager, "", null, null, "WEB-INF/queries/selectOperator.jsp");
		public static final UrlMap SelectOperator = new UrlMap(manager, "selectOperator", SelectOperatorPage, KUrls.Pages.Exception, "");

		public static final UrlMap SelectValuePage = new UrlMap(manager, "", null, null, "WEB-INF/queries/selectValue.jsp");
		public static final UrlMap SelectValue = new UrlMap(manager, "selectValue", SelectValuePage, KUrls.Pages.Exception, "");

		public static final UrlMap EvaluatePage = new UrlMap(manager, "", null, null, "WEB-INF/queries/evaluateQuery.jsp");
		public static final UrlMap Evaluate = new UrlMap(manager, "evaluate", EvaluatePage, KUrls.Pages.Exception, "");
		
		public static final UrlMap TestQueryPage = new UrlMap(manager, "", null, null, "WEB-INF/queries/evaluateQuery.jsp");
		public static final UrlMap TestQuery = new UrlMap(manager, "test", TestQueryPage, KUrls.Pages.Exception, "");

	}

	public static class Fuzzifications {
		public static String manager = "FuzzificationsManager";

		public static final UrlMap ListPage = new UrlMap(manager, "", null, null, "WEB-INF/fuzzifications/list.jsp");
		public static final UrlMap List = new UrlMap(manager, "list", ListPage, KUrls.Pages.Exception, "");

		public static final UrlMap EditPage = new UrlMap(manager, "", null, null, "WEB-INF/fuzzifications/edit.jsp");
		public static final UrlMap Edit = new UrlMap(manager, "edit", EditPage, KUrls.Pages.Exception, "");

		public static final UrlMap SavePage = new UrlMap(manager, "", null, null, "WEB-INF/fuzzifications/save.jsp");
		public static final UrlMap Save = new UrlMap(manager, "save", SavePage, KUrls.Pages.Exception, "");
	}

}
