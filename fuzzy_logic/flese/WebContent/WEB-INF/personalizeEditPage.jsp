<jsp:include page="commonHtmlHead.jsp" />

<%@page import="java.util.*"%>
<%@page import="java.io.*"%>
<%@page import="java.io.InputStreamReader"%>
<%@page import="auxiliar.ServletsAuxMethodsClass"%>
<%@page import="auxiliar.FunctionsClass"%>

<script type="text/javascript" src="js/jquery.jqplot.min.js"></script>
<link rel="stylesheet" type="text/css" href="js/jquery.jqplot.css" />
<script type="text/javascript" src="js/jqplot.canvasTextRenderer.min.js"></script>
<script type="text/javascript" src="js/jqplot.canvasAxisLabelRenderer.min.js"></script>
<script type="text/javascript" src="js/jqplot.highlighter.min.js"></script>
<script type="text/javascript" src="js/jqplot.cursor.min.js"></script>
<script type="text/javascript" src="js/jqplot.dateAxisRenderer.min.js"></script>

<script type="text/javascript">
	var functionValues = new Array();
	
	function drawChart(identifier) {
		
		/* $.jqplot('chartDiv_' + i,  [[[1, 2],[3,5.12],[5,13.1],[7,33.6],[9,85.9],[11,219.9]]]); */
		$.jqplot(identifier,  functionValues, {
		      // Give the plot a title.
		      // title: '',
		      // An axes object holds options for all axes.
		      // Allowable axes are xaxis, x2axis, yaxis, y2axis, y3axis, ...
		      // Up to 9 y axes are supported.
		      axes: {
		        // options for each axis are specified in seperate option objects.
		        xaxis: {
		          label: personalizePredInfo[i][1],
		          // Turn off "padding".  This will allow data point to lie on the
		          // edges of the grid.  Default padding is 1.2 and will keep all
		          // points inside the bounds of the grid.
		          pad: 0
		        },
		        yaxis: {
		          label: personalizePredInfo[i][0],
		          labelRenderer: $.jqplot.CanvasAxisLabelRenderer
		        }
		      },
			  highlighter: {
			       show: true,
			       sizeAdjust: 7.5
			  },
			  cursor: {
			       show: false
			  }
		});
	}
</script>

<script type="text/javascript">
	personalizePredInfo = new Array();
	<%
	String filePath = (String) request.getAttribute("filePath");
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
				FunctionsClass function = new FunctionsClass(line); 
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
	}
%>

	var personalizeServlet="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.PersonalizeServletEditAction, request, null)%>";
</script>

<body>
    <div id="bodyContainer">
    	<jsp:include page="commonBodyHead.jsp" />
    	<h3><a title="Back to the program files menu" href="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.FilesMgmtServlet, request, null) %>">Program Files Menu</a> &gt;
    		<a title="Back to personalize program file <%= (String) request.getAttribute("fileName") %>" href="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.PersonalizeServlet, request, null) %>?fileName=<%= (String) request.getAttribute("fileName") %>&fileOwner=<%= (String) request.getAttribute("fileOwner") %>">Personalize program file <%= (String) request.getAttribute("fileName") %></a> &gt; 
			Personalize fuzzification <%= (String) request.getAttribute("fuzzification") %>
    	</h3>
    	<table class="programFileChosen">
			<thead class="programFileChosen">
				<tr class="programFileChosen">
					<th colspan="2" class="programFileChosen">
						Information about the fuzzification chosen 
					</th>
				</tr>
			</thead>

			<tr>
				<td class="programFileChosen">Program file name</td>
				<td class="programFileChosen"><%= (String) request.getAttribute("fileName") %></td>
			</tr>
			<tr>
				<td class="programFileChosen">Owner</td>
				<td class="programFileChosen"><%= (String) request.getAttribute("fileOwner") %></td>
			</tr>
			<tr>
				<td class="programFileChosen">Fuzzification</td>
				<td class="programFileChosen"><%= (String) request.getAttribute("fuzzification") %></td>
			</tr>
			<tr>
				<td class="programFileChosen">Depends on the values of</td>
				<td class="programFileChosen" id="dependsOn"></td>
			</tr>
			
		</table>
    	
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
			
			var table = document.createElement('table');
			table.id = "myPersonalizationTable";
			table.className = "personalizationTable";
			divContainer.appendChild(table);

			var row = table.insertRow(-1);
			row.className = "personalizationTable";
			var cell = row.insertCell(-1);
			// cell.className = "personalizationTable";
			// cell.innerHTML=personalizePredInfo[i][2];
			cell.className = "personalizationGraphicsInTable";
			
			var chartDiv = document.createElement('div');
			chartDiv.id = 'chartDiv_' + i;
			cell.appendChild(chartDiv);
			// cell.innerHTML=personalizePredInfo[i][2];

			drawChart(i);
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
			var table = document.createElement('table');
			table.id = "personalizationTable";
			table.className = "personalizationTable";
			divContainer.appendChild(table);
		
			var row = table.insertRow(-1);
			row.className = "personalizationTable";
			var cell = null;
			cell = document.createElement('th');
			cell.className = "personalizationTable";
			cell.innerHTML = "Owner";
			row.appendChild(cell);
			cell = document.createElement('th');
			cell.className = "personalizationTable";
			cell.innerHTML = "";
			row.appendChild(cell);
			cell = document.createElement('th');
			cell.className = "personalizationGraphicsInTable";
			cell.innerHTML = "function applied";
			row.appendChild(cell);
			
			var chartDiv = null;
			for (var i=0; i<personalizePredInfo.length; i++) {
				row = table.insertRow(-1);
				row.className = "personalizationTable";
				if (personalizePredInfo[i].length >= 3) {
					if (personalizePredInfo[i][2] != fileOwner) {
						cell = row.insertCell(-1);
						cell.className = "personalizationTable";
						cell.innerHTML=personalizePredInfo[i][2];

						cell = row.insertCell(-1);
						cell.className = "personalizationTable";
						cell.innerHTML="<a title='take this function as my function' href='' onclick='return copyFunction("+i+")'><img src='images/copy.png'></img></a>";

						functionValues[0] = personalizePredInfo[i][3];
						
						cell = row.insertCell(-1);
						cell.className = "personalizationGraphicsInTable";
						cell.id = "functionGraphic_" + i;
						chartDiv = document.createElement('div');
						chartDiv.id = 'chartDiv_' + i;
						// cell.appendChild(chartDiv);
						// cell.innerHTML=personalizePredInfo[i][2];

						drawChart(cell.id);
						
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