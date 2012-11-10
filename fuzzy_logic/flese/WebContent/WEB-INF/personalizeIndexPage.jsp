<jsp:include page="commonHtmlHead.jsp" />

<%@page import="java.util.*"%>
<%@page import="java.io.*"%>
<%@page import="java.io.InputStreamReader"%>
<%@page import="auxiliar.ServletsAuxMethodsClass"%>

<script type="text/javascript">
	personalizePredInfo = new Array();
	<%
	String filePath = (String) request.getAttribute("filePath");
	if ((filePath != null) && ( ! ("".equals(filePath)))) {
		BufferedReader reader = new BufferedReader(new FileReader(filePath));
		String line;
		String whiteSpaces        = "[\\s]*";
		String anythingPattern    = "[\\s\\S]*";
		String termNamePattern    = "[0-9a-zA-Z_-]+";
		String predicatePattern   = whiteSpaces + termNamePattern + "[\\(]{1}" + whiteSpaces + 
									termNamePattern + whiteSpaces + "[\\)]{1}" + whiteSpaces;
		String prologIfPattern    = whiteSpaces + "[:]{1}[-]{1}" + whiteSpaces;
		String floatPattern       = whiteSpaces + "[\\d]+(\\.[\\d]+)?" + whiteSpaces;
		String listElementPattern = whiteSpaces + "[\\(]{1}"+floatPattern+"[,]{1}"+floatPattern+"[\\)]{1}"+
									whiteSpaces + "[,]?" + whiteSpaces;
		String functionPattern    = "(function\\(\\[){1}" + "["+listElementPattern+"]{2,}" + "(\\]\\)){1}"; 
		String detectionPattern   = "^(rfuzzy_fuzzification\\(){1}("+predicatePattern+"){1}[,]{1}("+predicatePattern+"){1}[\\)]{1}"+
									prologIfPattern + "(" + functionPattern + "){1}" + whiteSpaces + 
									"(\\.){1}" + anythingPattern + "$";
		
		int i=0;
		while ((line = reader.readLine()) != null) {
			out.println("// " + line + "\n");
			if (line.matches(detectionPattern)) {
				String lineStart             = line.replaceAll(detectionPattern, "$1");
				String predNameDefined       = line.replaceAll(detectionPattern, "$2");
				String predNameNecessary     = line.replaceAll(detectionPattern, "$3");
				String predNameFunction      = line.replaceAll(detectionPattern, "$4");
				String predNameFunctionStart = line.replaceAll(detectionPattern, "$5");
				String predNameFunctionEnd   = line.replaceAll(detectionPattern, "$6");
				String lineEnd               = line.replaceAll(detectionPattern, "$7");
				out.println("// lineStart: "             + lineStart + "\n");
				out.println("// predNameDefined: "       + predNameDefined + "\n");
				out.println("// predNameNecessary: "     + predNameNecessary + "\n");
				out.println("// predNameFunction: "      + predNameFunction + "\n");
				out.println("// predNameFunctionStart: " + predNameFunctionStart + "\n");
				out.println("// predNameFunctionEnd: "   + predNameFunctionEnd + "\n");
				out.println("// lineEnd: "               + lineEnd + "\n");
				
				out.println("personalizePredInfo["+i+"]= new Array('" + 
							predNameDefined + "', '" + predNameNecessary + "', '" + predNameFunction + "', '" + 
							line + "'); \n");
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
					cell.innerHTML=personalizePredInfo[i][2];

				}
			}
		}
	</script>
</body>
</html>