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
	String mode = requestStoreHouse.getRequestParameter(KConstants.Request.mode);
	String [] fuzzifications = resultsStoreHouse.getFuzzificationsList();
	
	String predDefined = requestStoreHouse.getRequestParameter(KConstants.Fuzzifications.predDefined);
	String predNecessary = requestStoreHouse.getRequestParameter(KConstants.Fuzzifications.predNecessary);
	
%>

<div class='personalizationDivFuzzificationFunctionTable'>
	<% if (mode.equals(KConstants.Request.modeAdvanced)) { %>
	<div class='personalizationDivFuzzificationFunctionTableRow'>
		<div class='personalizationDivFuzzificationFunctionTableCell1' id='<%=KConstants.JspsDivsIds.fuzzificationGraphicDivId %>'>
		
		</div>
	</div>
	<% } %>
	<div class='personalizationDivFuzzificationFunctionTableRow'>
		<div class='personalizationDivFuzzificationFunctionTableCell2' id='<%=KConstants.JspsDivsIds.fuzzificationValuesAndButtonDivId %>'>
			<div class='personalizationDivFuzzificationFunctionWithButtonTable'>
				<div class='personalizationDivFuzzificationFunctionWithButtonTableRow'>
					<div class='personalizationDivFuzzificationFunctionWithButtonTableCell'>
						<div class='personalizationDivFuzzificationFunctionValuesTable'>
							<div class='personalizationDivFuzzificationFunctionValuesTableRow'>
								<div class='personalizationDivFuzzificationFunctionValuesTableCell'>
									A <%= JspsUtils.getPrologNameAsFuzzificationFunctionName(predDefined, true) %> 
									whose value for <%= JspsUtils.getPrologNameAsFuzzificationFunctionName(predNecessary, false) %>
									is
								</div>
								<div class='personalizationDivFuzzificationFunctionValuesTableCell'>
									is <%= JspsUtils.getPrologNameAsFuzzificationFunctionName(predDefined, false) %>
									with a degree of
								</div>
								<div class='personalizationDivFuzzificationFunctionValuesTableCell'>
									Current Value
								</div>
								<div class='personalizationDivFuzzificationFunctionValuesTableCell'>
									<% if (mode.equals(KConstants.Request.modeAdvanced)) { %>
										Old Value
									<% } else { %>
										Default Value
									<% } %>
								</div>
							</div>
						</div>
					</div>
				</div>
				<div class='personalizationDivFuzzificationFunctionWithButtonTableRow'>
					<div class='personalizationDivFuzzificationFunctionWithButtonTableCell'>
					</div>
				</div>
			</div>
		</div>
	</div>
</div>


<script type="text/javascript">
<%	
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
<% if (mode.equals(KConstants.Request.modeAdvanced)) { %>
	insertFuzzificationGraphicRepresentation('<%=KConstants.JspsDivsIds.fuzzificationGraphicDivId %>');
<% } %>
</script>



<!-- END -->