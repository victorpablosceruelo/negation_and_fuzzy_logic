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
	String mode = requestStoreHouse.getRequestParameter(KConstants.Request.mode);
	ProgramPartAnalysis [][] fuzzifications = resultsStoreHouse.getProgramPartAnalysis();

	ProgramPartAnalysis myFuzzification = null;
	ProgramPartAnalysis defaultFuzzification = null;

	if ((fuzzifications.length == 1)) {
		if (fuzzifications[0].length == 1) {
			defaultFuzzification = fuzzifications[0][0];
			myFuzzification = fuzzifications[0][0];			
		}
		if (fuzzifications[0].length == 2) {
			if (fuzzifications[0][0].getPredOwner().equals(KConstants.Fuzzifications.DEFAULT_DEFINITION)) {
				defaultFuzzification = fuzzifications[0][0];
				myFuzzification = fuzzifications[0][1];
			}
			else {
				defaultFuzzification = fuzzifications[0][1];
				myFuzzification = fuzzifications[0][0];			
			}
		}
	}
	
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
									A <%= JspsUtils.getFromFuzzificationNameOf(defaultFuzzification, KConstants.Fuzzifications.database, true) %> 
									whose value for <%= JspsUtils.getFromFuzzificationNameOf(defaultFuzzification, KConstants.Fuzzifications.predNecessary, true) %>
									is
								</div>
								<div class='personalizationDivFuzzificationFunctionValuesTableCell'>
									is <%= JspsUtils.getFromFuzzificationNameOf(defaultFuzzification, KConstants.Fuzzifications.predDefined, true) %>
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