<%@page import="constants.KUrls"%>
<%@page import="filesAndPaths.ProgramFileInfo"%>
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
	ProgramFileInfo programFileInfo = resultsStoreHouse.getProgramFileInfo();
	ProgramPartAnalysis [][] fuzzifications = resultsStoreHouse.getProgramPartAnalysis();
	String mode = requestStoreHouse.getRequestParameter(KConstants.Request.mode);
	String urlEditFuzzification = KUrls.Fuzzifications.Edit.getUrl(true);
	
	if (fuzzifications.length != 0) {
%>

<div class='personalizationDivMainTable'>
	<div class='personalizationDivMainTableRow'>
		<div class='personalizationDivMainTableCell'>
			<div class='personalizationDivSelectFuzzificationTable'>
				<div class='personalizationDivSelectFuzzificationTableRow'>
					<div class='personalizationDivSelectFuzzificationTableCell'>
						I want to personalize how it is determined that a &nbsp;
					</div>
					<div class='personalizationDivSelectFuzzificationTableCell'>
						<select name="personalizationSelectComboBoxId" id="personalizationSelectComboBoxId"
								onchange="personalizationFunctionChanged(this, '<%=KConstants.JspsDivsIds.personalizationFunctionUnderModificationDivId %>', '<%= urlEditFuzzification %>');">
								<%=JspsUtils.comboBoxDefaultValue() %>
<%
								for (int i=0; i<fuzzifications.length; i++) {
									if ((fuzzifications[i] != null) && (fuzzifications[i].length > 0)) {
										ProgramPartAnalysis fuzzification = fuzzifications[i][0];
										String desc = JspsUtils.getFromFuzzificationNameOf(fuzzification, KConstants.Fuzzifications.database, true) +
														" is " + 
														JspsUtils.getFromFuzzificationNameOf(fuzzification, KConstants.Fuzzifications.predDefined, true) +
														" from the value it has for " + 
														JspsUtils.getFromFuzzificationNameOf(fuzzification, KConstants.Fuzzifications.predNecessary, true);
										String id = "&" + KConstants.Request.fileOwnerParam + "=" + programFileInfo.getFileOwner() +
													"&" + KConstants.Request.fileNameParam + "=" + programFileInfo.getFileName() + 
													"&" + KConstants.Fuzzifications.predDefined + "=" + fuzzification.getPredDefined() +
													"&" + KConstants.Fuzzifications.predNecessary + "=" + fuzzification.getPredNecessary() +
													"&" + KConstants.Request.mode + "=" + mode;
%>
									<option id='<%=desc%>' title='<%=desc%>' value='<%=id%>'><%= desc %></option>
<%
									}
								}
%>
						</select>
					</div>
				</div> 
			</div>
		</div>
	</div>
	<div class='personalizationDivMainTableRow'>
		<div class='personalizationDivMainTableCell' id='<%=KConstants.JspsDivsIds.personalizationFunctionUnderModificationDivId %>'>
			Select the fuzzification you want to personalize.
		</div>
	</div>
</div>

<%
	} else {
%>
<div class='personalizationDivMainTable'>
	<div class='personalizationDivMainTableRow'>
		<div class='personalizationDivMainTableCell'>
			You cannot personalize this program file.
		</div>
	</div>
</div>

<% } %>
<script type="text/javascript">
</script>



<!-- END -->