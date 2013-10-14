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

<script type="text/javascript">
<%
	RequestStoreHouse requestStoreHouse = new RequestStoreHouse(request);
	ResultsStoreHouse resultsStoreHouse = JspsUtils.getResultsStoreHouse(request);
	ProgramFileInfo programFileInfo = resultsStoreHouse.getProgramFileInfo();
	ProgramPartAnalysis [][] fuzzifications = resultsStoreHouse.getProgramPartAnalysis();
	
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
								onchange="personalizationFunctionChanged(this, '<%=KConstants.JspsDivsIds.personalizationFunctionUnderModificationDivId %>',  
														'advanced', fileName, fileOwner);">
								<%=JspsUtils.comboBoxDefaultValue() %>
<%
								for (int i=0; i<fuzzifications.length; i++) {
									if (fuzzifications[i].length > 0) {
										ProgramPartAnalysis fuzzification = fuzzifications[i][0];
										String desc = JspsUtils.getPrologNameInColloquialLanguage(fuzzification.getPredDefined()) + 
														" from the value it has for " + 
														JspsUtils.getPrologNameInColloquialLanguage(fuzzification.getPredNecessary());
										String id = KConstants.Request.fileOwnerParam + "=" + programFileInfo.getFileOwner() +
													"&" + KConstants.Request.fileNameParam + "=" + programFileInfo.getFileName() + 
													"&" + KConstants.Fuzzifications.predDefined + "=" + fuzzification.getPredDefined() +
													"&" + KConstants.Fuzzifications.predNecessary + "=" + fuzzification.getPredNecessary();
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
		<div class='personalizationDivMainTableCell'>
			Select the fuzzification you want to personalize.
		</div>
	</div>
</div>

<script type="text/javascript">
	<% if (fuzzifications.length == 0) { %>
		alert("The program has nothing to personalize.");
	<% } %>
</script>



<!-- END -->