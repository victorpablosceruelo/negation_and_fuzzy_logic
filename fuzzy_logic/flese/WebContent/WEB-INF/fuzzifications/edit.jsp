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
	String predDefined = requestStoreHouse.getRequestParameter(KConstants.Fuzzifications.predDefined);
	String predNecessary = requestStoreHouse.getRequestParameter(KConstants.Fuzzifications.predNecessary);
	
	String saveUrl = KUrls.Fuzzifications.Save.getUrl(true) + 
			"&" + KConstants.Request.fileNameParam + "=" + fileName + 
			"&" + KConstants.Request.fileOwnerParam + "=" + fileOwner +
			"&" + KConstants.Request.mode + "=" + mode +
			"&" + KConstants.Fuzzifications.predDefined + "=" + predDefined +
			"&" + KConstants.Fuzzifications.predNecessary + "=" + predNecessary;
	JspsUtils.getValue(saveUrl);
	
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
	String[][] points = new String[defFuzzPoints.length][];
	
	for (int i=0; i<defFuzzPoints.length; i++) {
		FunctionPoint defFuzzPoint = defFuzzPoints[i];
		FunctionPoint myFuzzPoint = null;
		int j=0;
		while ((j<myFuzzPoints.length) && (myFuzzPoint == null)) {
			if (defFuzzPoint.getCoordinate1().equals(myFuzzPoints[j].getCoordinate1())) {
				myFuzzPoint = myFuzzPoints[j];
			}
			else j++;
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

							<% for (int i=0; i<points.length; i++) { 
									String fuzzificationBarDivId = KConstants.JspsDivsIds.fuzzificationBarValueDivId + "[" + i + "]";
							%>
							<div class='personalizationDivFuzzificationFunctionValuesTableRow'>
								<div class='personalizationDivFuzzificationFunctionValuesTableCell'>
									<%= points[i][0] %>
								</div>
								<div class='personalizationDivFuzzificationFunctionValuesTableCell'>
									<input type='hidden' name='fuzzificationBars[<%= i %>].fpx' value='<%= points[i][0] %>'/>
						 			<input type='range'  name='fuzzificationBars[<%= i %>].fpy' min='0' max='1' 
						 					step='0.01' value='<%= points[i][1] %>' width='150px' 
						 					onchange="barValueChanged(this, '<%=fuzzificationBarDivId %>', '<%= points[i][0] %>', '<%= KConstants.JspsDivsIds.fuzzificationGraphicDivId %>');"/>
								</div>
								<div class='personalizationDivFuzzificationFunctionValuesTableCell'>
									<span id='<%=fuzzificationBarDivId %>'><%= points[i][1] %></span>
								</div>
								<div class='personalizationDivFuzzificationFunctionValuesTableCell'>
									<%= points[i][2] %>
								</div>
							</div>
							<% } %>
						</div>
					</div>
				</div>

				<div class='personalizationDivFuzzificationFunctionWithButtonTableRow'>
					<div class='personalizationDivFuzzificationFunctionWithButtonTableCell'>
						<div class='personalizationDivSaveButtonAndMsgTable'>
							<div class='personalizationDivSaveButtonAndMsgTableRow'>
								<div class='personalizationDivSaveButtonAndMsgTableCell'>
									<INPUT type='submit' value='Save modifications' 
											onclick="saveFuzzification('<%=KConstants.JspsDivsIds.fuzzificationSaveStatusDivId %>', '<%=saveUrl %>')">
								</div>
								<div class='personalizationDivSaveButtonAndMsgTableCell'>
									&nbsp;&nbsp;&nbsp;&nbsp;
								</div>
								<div class='personalizationDivSaveButtonAndMsgTableCell' id='<%=KConstants.JspsDivsIds.fuzzificationSaveStatusDivId %>'>
								</div> 
							</div>
						</div>
					</div>
				</div>
			</div>
		</div>
	</div>
</div>

<script type="text/javascript">
<%
	String dbPredIsPredDefined = JspsUtils.getFromFuzzificationNameOf(defaultFuzzification, KConstants.Fuzzifications.database, true) + " is " +
			JspsUtils.getFromFuzzificationNameOf(defaultFuzzification, KConstants.Fuzzifications.predDefined, true);
	String PredNecessaryOfADbPred = JspsUtils.getFromFuzzificationNameOf(defaultFuzzification, KConstants.Fuzzifications.predNecessary, true) +
			" of a " + JspsUtils.getFromFuzzificationNameOf(defaultFuzzification, KConstants.Fuzzifications.database, true);
	
	String defaultDefinitionName = defaultFuzzification.getPredOwner();
	String myName = localUserInfo.getLocalUserName();
	
	if (mode.equals(KConstants.Request.modeAdvanced)) {
		defaultDefinitionName = KConstants.Fuzzifications.PREVIOUS_DEFAULT_DEFINITION;
		myName = KConstants.Fuzzifications.DEFAULT_DEFINITION;
	}

	out.print("setFuzzificationFunction('" + defaultFuzzification.getPredDefined() + "', '" + defaultFuzzification.getPredNecessary());
	out.print("', 0, '" + dbPredIsPredDefined + "', '" + PredNecessaryOfADbPred + "', new Array("); 
	out.print("new fuzzificationPoints('"+ myName + "', '" + myName + "', new Array(");
	
	for (int i=0; i<points.length; i++) {
		out.print("new Array(" + points[i][0] + ", " + points[i][1] + ")");
		if (i+1 < points.length) {
			out.print(", ");
		}
	} 
	out.print(")), ");
	
	out.print("new fuzzificationPoints('" + defaultDefinitionName + "', '" + defaultDefinitionName + "', new Array(");
	for (int i=0; i<points.length; i++) {
		out.print("new Array(" + points[i][0] + ", " + points[i][2] + ")");
		if (i+1 < points.length) {
			out.print(", ");
		}
	} 
	out.print("))));");
%>
	insertFuzzificationGraphicRepresentation('<%= KConstants.JspsDivsIds.fuzzificationGraphicDivId %>');
</script>

<!-- END -->