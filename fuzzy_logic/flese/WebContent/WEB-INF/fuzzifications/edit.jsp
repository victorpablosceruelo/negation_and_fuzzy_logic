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
	
	FunctionPoint [] defFuzzPoints = defaultFuzzification.getFunctionPoints();
	FunctionPoint [] myFuzzPoints = myFuzzification.getFunctionPoints(); 
	String[][] points = new String[defFuzzPoints.length];
	
	for (int i=0; i<defFuzzPoints.length; i++) {
		FunctionPoint defFuzzPoint = defFuzzPoints[i];
		FunctionPoint myFuzzPoint = null;
		int j=0;
		while ((j<myFuzzPoints.length) && (myFuzzPoint == null)) {
			if (defFuzzPoint.getCoordinate1().equals(myFuzzPoints[j].getCoordinate1())) {
				myFuzzPoint = myFuzzPoints[j];
			}
		}
		if ((myFuzzPoint == null) || (mode.equals(KConstants.Request.modeAdvanced))) {
			myFuzzPoint = defFuzzPoint;
		}
		String fpx = defFuzzPoint.getCoordinate1();
		String fpy = defFuzzPoint.getCoordinate2();
		String fpd = myFuzzPoint.getCoordinate2();

		points[i] = new String[3];
		points[i][0] = fpx;
		points[i][1] = fpy;
		points[i][2] = fpd;
	}
	
%>
<script type="text/javascript">
<%

	out.print("addFuzzificationFunction('" + localUserInfo.getLocalUserName() + "', new Array(");
	for (int i=0; i<points.length; i++) {
		out.print("new Array(" + points[i][0] + ", " + points[i][1] + ")");
		if (i+1 < points.length) {
			out.print(", ");
		}
	} 
	out.print(");");
	
	out.print("addFuzzificationFunction('" + defaultFuzzification.getPredOwner() + "', new Array(");
	for (int i=0; i<points.length; i++) {
		out.print("new Array(" + points[i][0] + ", " + points[i][2] + ")");
		if (i+1 < points.length) {
			out.print(", ");
		}
	} 
	out.print(");");
	
%>
</script>

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

							<div class='personalizationDivFuzzificationFunctionValuesTableRow'>
								<div class='personalizationDivFuzzificationFunctionValuesTableCell'>
									<%= fpx %>
								</div>
								<div class='personalizationDivFuzzificationFunctionValuesTableCell'>
									<input type='hidden' name='fuzzificationBars[<%= i %>].fpx' value='<%=fpx %>'/>
						 			<input type='range'  name='fuzzificationBars[<%= i %>].fpy' min='0' max='1' step='0.01' value='"+fpy+"' width='150px' 
						 					"onchange="barValueChanged(this, '<%= i %>', "+indexOfMine+", "+index+", \""+fuzzificationGraphicDivId+"\")'/>
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


<!-- END -->