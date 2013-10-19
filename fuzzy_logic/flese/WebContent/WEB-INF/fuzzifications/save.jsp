<%@page import="constants.KUrls"%>
<%@page import="auxiliar.LocalUserInfo"%>
<%@page import="auxiliar.FunctionPoint"%>
<%@page import="auxiliar.ProgramPartAnalysis"%>
<%@page import="constants.KConstants"%>
<%@page import="auxiliar.JspsUtils"%>
<%@page import="storeHouse.RequestStoreHouse"%>
<%@page import="results.ResultsStoreHouse"%>
<%@page import="java.util.*"%>
<%@page import="java.io.*"%>
<%@page import="java.io.InputStreamReader"%>
<%@page import="auxiliar.ProgramAnalysisClass"%>

<%
	RequestStoreHouse requestStoreHouse = new RequestStoreHouse(request);
	ResultsStoreHouse resultsStoreHouse = JspsUtils.getResultsStoreHouse(request);
	LocalUserInfo localUserInfo = requestStoreHouse.getSession().getLocalUserInfo();
	String mode = requestStoreHouse.getRequestParameter(KConstants.Request.mode);
	String fileName = requestStoreHouse.getRequestParameter(KConstants.Request.fileNameParam);
	String fileOwner = requestStoreHouse.getRequestParameter(KConstants.Request.fileOwnerParam);
	String predDefined = requestStoreHouse.getRequestParameter(KConstants.Request.fileOwnerParam);
	String predNecessary = requestStoreHouse.getRequestParameter(KConstants.Request.fileOwnerParam);
	String predOwner = requestStoreHouse.getRequestParameter(KConstants.Request.fileOwnerParam);
	
%>

Updated file <%=fileName %>.

<!-- END -->