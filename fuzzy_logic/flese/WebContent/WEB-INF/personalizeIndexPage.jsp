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

<script type="text/javascript">
	function drawChart(i) {
		var myFunction = personalizePredInfo[i][3];
		
		// $.jqplot('chartDiv_' + i,  [[[1, 2],[3,5.12],[5,13.1],[7,33.6],[9,85.9],[11,219.9]]]);
		$.jqplot('chartDiv_' + i,  [myFunction]);
	}
</script>

<script type="text/javascript">
	personalizePredInfo = new Array();
	<%
	String filePath = (String) request.getAttribute("filePath");
	if ((filePath != null) && ( ! ("".equals(filePath)))) {
		BufferedReader reader = new BufferedReader(new FileReader(filePath));
		
		String line;
		int i=0;
		while ((line = reader.readLine()) != null) {
			out.println("// " + line + "\n");
			if (line.startsWith("rfuzzy_fuzzification")) {
				FunctionsClass function = new FunctionsClass(line); 
				out.println("// line: "             + line + "\n");
				out.println("personalizePredInfo["+i+"]= new Array('" + 
							function.getPredDefined() + "', '" + function.getPredNecessary() + "', '" + function.getPredOwner() + "', " + 
							function.getFunctionInJavaScript() + "); \n");
				// out.println(line);
				// out.print("<br />\n");
				i++;
			}
		}
		reader.close();
	}
%>

	var personalizeServlet="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.PersonalizeServletEditAction, request, null)%>";
</script>

<body>
    <div id="body">
    	<jsp:include page="commonBodyHead.jsp" />
    	<h3><a title="Back to the program files menu" href="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.FilesMgmtServlet, request, null) %>">Program Files Menu</a> &gt; 
    		View program file <%= (String) request.getAttribute("fileName") %> </h3>
		<br />
		<br />
		<div id="personalizationTableDiv"></div><table id="resultsTable" class="personalizationTable"></table>
		<br />
		<br />
		<h3><a title="Back to the program files menu" href="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.FilesMgmtServlet, request, null) %>">Program Files Menu</a> &gt; 
    		View program file <%= (String) request.getAttribute("fileName") %> </h3>
    	<br /><br />
	</div>
	<script type="text/javascript">
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
			cell.innerHTML = "Fuzzification";
			row.appendChild(cell);
			cell = document.createElement('th');
			cell.className = "personalizationTable";
			cell.innerHTML = "depends on the values of";
			row.appendChild(cell);
			cell = document.createElement('th');
			cell.className = "personalizationTable";
			cell.innerHTML = "applying function";
			row.appendChild(cell);
			
			var chartDiv = null;
			for (var i=0; i<personalizePredInfo.length; i++) {
				row = table.insertRow(-1);
				row.className = "personalizationTable";
				if (personalizePredInfo[i].length >= 3){

					cell = row.insertCell(-1);
					cell.className = "personalizationTable";
					cell.innerHTML=personalizePredInfo[i][0];

					cell = row.insertCell(-1);
					cell.className = "personalizationTable";
					cell.innerHTML=personalizePredInfo[i][1];

					cell = row.insertCell(-1);
					cell.className = "personalizationTable";
					chartDiv = document.createElement('div');
					chartDiv.id = 'chartDiv_' + i;
					cell.appendChild(chartDiv);
					// cell.innerHTML=personalizePredInfo[i][2];

					drawChart(i);
				}
			}
		}
	</script>
</body>
</html>