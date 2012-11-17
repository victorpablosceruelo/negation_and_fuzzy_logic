<jsp:include page="commonHtmlHead.jsp" />

<%@page import="java.util.*"%>
<%@page import="java.io.*"%>
<%@page import="java.io.InputStreamReader"%>
<%@page import="auxiliar.ServletsAuxMethodsClass"%>
<%@page import="auxiliar.FunctionClass"%>

<script type="text/javascript" src="js/highcharts.js" ></script>

<script type="text/javascript">
	var functionValues = new Array();
	var charts = new Array(); // globally available
	
	function drawChart(identifier, index) {
		
		$(document).ready(function() {
		      charts[i] = new Highcharts.Chart({
		         chart: {
		            renderTo: identifier,
		            type: 'bar'
		         },
		         title: {
		            text: 'Fruit Consumption'
		         },
		         xAxis: {
		            categories: ['Apples', 'Bananas', 'Oranges']
		         },
		         yAxis: {
		            title: {
		               text: 'Fruit eaten'
		            }
		         },
		         series: [{
		            name: 'Jane',
		            data: [1, 0, 4]
		         }, {
		            name: 'John',
		            data: [5, 7, 3]
		         }]
		      });
		   });
	}
</script>

<script type="text/javascript">
	personalizePredInfo = new Array();
	<%String filePath = (String) request.getAttribute("filePath");
	String fuzzification = (String) request.getAttribute("fuzzification");
	
	if ((filePath != null) && ( ! ("".equals(filePath))) && 
			(fuzzification != null) && ( ! ("".equals(fuzzification)))) {
		BufferedReader reader = new BufferedReader(new FileReader(filePath));
		
		out.println("// fuzzification: " + fuzzification + "\n");
		
		String line;
		int i=0;
		while ((line = reader.readLine()) != null) {
			out.println("// " + line + "\n");
			if (line.startsWith("rfuzzy_fuzzification")) {
				FunctionClass function = new FunctionClass(line); 
				if (fuzzification.equals(function.getPredDefined())) {
					out.println("// line: "             + line + "\n");
					out.println("personalizePredInfo["+i+"]= new Array('" + 
								function.getPredDefined() + "', '" + function.getPredNecessary() + "', '" + function.getPredOwner() + "', " + 
								function.getFunctionInJavaScript() + "); \n");
					// out.println(line);
					// out.print("<br />\n");
					i++;
				}
				else {
					out.println("// function.getPredDefined(): " + function.getPredDefined() + "\n");
				}
			}
		}
		reader.close();
	}%>

	var personalizeServlet="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.PersonalizeServletEditAction, request, null)%>";
</script>

<body>
    <div id="bodyContainer">
    	<jsp:include page="commonBodyHead.jsp" />
    	<h3><a title="Back to the program files menu" href="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.FilesMgmtServlet, request, null) %>">Program Files Menu</a> &gt;
    		<a title="Back to personalize program file <%= (String) request.getAttribute("fileName") %>" href="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.PersonalizeServlet, request, null) %>?fileName=<%= (String) request.getAttribute("fileName") %>&fileOwner=<%= (String) request.getAttribute("fileOwner") %>">Personalize program file <%= (String) request.getAttribute("fileName") %></a> &gt; 
			Personalize fuzzification <%= (String) request.getAttribute("fuzzification") %>
    	</h3>
    	<div class="tableWithBorderWidth50">
			<div class="tableHeadWithOutBorderWidth100">
					Information about the fuzzification chosen
			</div>

			<div class="tableRowWithBorderWidth100">
				<div class="tableCellWithBorderWidth50">Program file name</div>
				<div class="tableCellWithBorderWidth50"><%= (String) request.getAttribute("fileName") %></div>
			</div>
			<div class="tableRowWithBorderWidth100">
				<div class="tableCellWithBorderWidth50">Owner</div>
				<div class="tableCellWithBorderWidth50"><%= (String) request.getAttribute("fileOwner") %></div>
			</div>
			<div class="tableRowWithBorderWidth100">
				<div class="tableCellWithBorderWidth50">Fuzzification</div>
				<div class="tableCellWithBorderWidth50"><%= (String) request.getAttribute("fuzzification") %></div>
			</div>
			<div class="tableRowWithBorderWidth100">
				<div class="tableCellWithBorderWidth50">Depends on the values of</div>
				<div class="tableCellWithBorderWidth50" id="dependsOn"></div>
			</div>			
		</div>
    	
		<br />
		<br />
		<div id="personalizationTableDiv"></div>
		<br />
		<br />
		My personalized fuzzification
		<div id="myPersonalizationTableDiv">You have not defined your personalization for this fuzzification yet.</div>
		<br />
		<br />		
    	<h3><a title="Back to the program files menu" href="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.FilesMgmtServlet, request, null) %>">Program Files Menu</a> &gt;
    		<a title="Back to personalize program file <%= (String) request.getAttribute("fileName") %>" href="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.PersonalizeServlet, request, null) %>?fileName=<%= (String) request.getAttribute("fileName") %>&fileOwner=<%= (String) request.getAttribute("fileOwner") %>">Personalize program file <%= (String) request.getAttribute("fileName") %></a> &gt; 
			Personalize fuzzification <%= (String) request.getAttribute("fuzzification") %>
    	</h3>
    	<br /><br />
	</div>
	<script type="text/javascript">

		var myPersonalizedFunction = new Array();
		
		function createPersonalizationTable() {
			var divContainer = document.getElementById("myPersonalizationTableDiv");
			divContainer.innerHTML = ""; // clean up !!!
			
			var table = document.createElement('div');
			table.id = "myPersonalizationTable";
			table.className = "personalizationTable";
			divContainer.appendChild(table);

			var row = document.createElement('div');
			row.className = "personalizationTableRow";
			table.appendChild(row);
			
			var cell = document.createElement('div');
			row.appendChild(cell);
			// cell.className = "personalizationTable";
			// cell.innerHTML=personalizePredInfo[i][2];
			cell.className = "personalizationTableCell3";
			cell.id = 'chartDiv_' + i;
			// cell.innerHTML=personalizePredInfo[i][2];

			// drawChart(cell.id, i);
			// Cannot use i here !!!
		}
		function updateFunctionTable() {
			
		}
		
		function updateFunctionGraphic() {
			
		}
		
		function copyFunctionValues(index) {
			newFunctionValues = personalizePredInfo[index][3];
			for (var i=0; i<newFunctionValues.length; i++) {
				myPersonalizedFunction[i] = new Array();
				for (var j=0; j<newFunctionValues[i].length; j++) {
					if (j>1) {
						alert("Function is not well defined. Extra information will be discarded.")
					}
					else {
						myPersonalizedFunction[i][j] = newFunctionValues[i][j];
					}
				}
			}
			updateFunctionTable();
			updateFunctionGraphic();
		}
		
		function copyFunction(index) {
			var functionOwner = personalizePredInfo[index][2];
			var confirmationText="";
			if (functionOwner == '') {
				confirmationText = "predefined function";
			}
			else {
				confirmationText = "function defined by " + functionOwner;
			}
			if (confirm("Do you want to take the " + confirmationText + " as your personalized function for the fuzzification " + personalizePredInfo[index][0])) {
				copyFunctionValues(index);
			}
			return false;
		}
	
	
		var personalizeServletEditAction = "<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.PersonalizeServletEditAction, request, null) %>";
		var fileName = "<%= (String) request.getAttribute("fileName") %>";
		var fileOwner = "<%= (String) request.getAttribute("fileOwner") %>";
		var myPersonalization = null;
		
		if (personalizePredInfo.length > 0) {
			var divContainer = document.getElementById("personalizationTableDiv");
			var table = document.createElement('div');
			table.id = "personalizationTable";
			table.className = "personalizationTable";
			divContainer.appendChild(table);
		
			var row = document.createElement('div');
			row.className = "personalizationTableRow";
			table.appendChild(row);
			
			var cell = null;
			cell = document.createElement('div');
			cell.className = "personalizationTableCell1";
			cell.innerHTML = "Owner";
			row.appendChild(cell);
			
			cell = document.createElement('div');
			cell.className = "personalizationTableCell2";
			cell.innerHTML = "";
			row.appendChild(cell);
			
			cell = document.createElement('div');
			cell.className = "personalizationTableCell3";
			cell.innerHTML = "function applied";
			row.appendChild(cell);
			
			var chartDiv = null;
			for (var i=0; i<personalizePredInfo.length; i++) {
				row = document.createElement('div');
				row.className = "personalizationTableRow";
				table.appendChild(row);
				
				if (personalizePredInfo[i].length >= 3) {
					if (personalizePredInfo[i][2] != fileOwner) {
						cell = document.createElement('div');
						cell.className = "personalizationTableCell1";
						cell.innerHTML=personalizePredInfo[i][2];
						row.appendChild(cell);

						cell = document.createElement('div');
						cell.className = "personalizationTableCell2";
						cell.innerHTML="<a title='take this function as my function' href='' onclick='return copyFunction("+i+")'><img src='images/copy.png'></img></a>";
						row.appendChild(cell);

						functionValues[0] = personalizePredInfo[i][3];
						
						cell = document.createElement('div');
						cell.className = "personalizationTableCell3";
						cell.id = "functionGraphic_" + i;
						row.appendChild(cell);
						// cell.innerHTML=personalizePredInfo[i][2];

						drawChart(cell.id, i);
						
						document.getElementById("dependsOn").innerHTML = personalizePredInfo[i][1];
					}
					else {
						myPersonalization = i;
					}
				}
			}
		}
		
		if (myPersonalization != null) {
			copyFunctionValues(myPersonalization);
		}
		
	</script>
</body>
</html>