<%@page import="constants.KConstants"%>
<%@page import="auxiliar.JspsUtils"%>
<%@page import="storeHouse.RequestStoreHouse"%>
<%@page import="results.ResultsStoreHouse"%>
<%@page import="java.util.*"%>
<%@page import="java.io.*"%>
<%@page import="java.io.InputStreamReader"%>
<%@page import="auxiliar.ProgramAnalysisClass"%>

<script type="text/javascript">
<%
	RequestStoreHouse requestStoreHouse = new RequestStoreHouse(request);
	ResultsStoreHouse resultsStoreHouse = JspsUtils.getResultsStoreHouse(request);
	String [] fuzzifications = resultsStoreHouse.getFuzzificationsList();
	
	out.println("cleanUpFuzzificationFunctionsDefinitions();");
	if (fuzzifications != null) {
		for (int i=0; i<fuzzifications.length; i++) {
			out.println(fuzzifications[i]);
		}
		out.println("showPersonalizeProgramFileDialog(fileName, fileOwner, mode);");
	}
	else {
		out.println("addMsgToTheUser('ERROR: fuzzifications is null.');");
	}
/*	else {
		out.println("addMsgToTheUser('ERROR: programAnalized is null.');");
	}
*/
%>
</script>

<!-- END -->