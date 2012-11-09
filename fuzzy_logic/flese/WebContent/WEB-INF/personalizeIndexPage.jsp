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
		String listElementPattern = whiteSpaces + "\\("+floatPattern+"[,]{1}"+floatPattern+"\\)"+
									whiteSpaces + "[,]*" + whiteSpaces;
		String functionPattern    = "function\\(\\[" + "("+listElementPattern+")+" + "\\]\\)"; 
		String detectionPattern   = "^rfuzzy_fuzzification\\(("+predicatePattern+"){1}[,]{1}("+predicatePattern+"){1}[\\)]{1}"+
									prologIfPattern + functionPattern + whiteSpaces + "(\\.){1}" + anythingPattern + "$";
		
		int i=0;
		while ((line = reader.readLine()) != null) {
			if (line.matches(detectionPattern)) {
				String predNameDefined = line.replaceAll(detectionPattern, "$1");
				String predNameNecessary = line.replaceAll(detectionPattern, "$2");
				String predNameFunction1 = line.replaceAll(detectionPattern, "$3");
				String predNameFunction2 = line.replaceAll(detectionPattern, "$4");
				
				out.println("personalizePredInfo["+i+"]= new Array('" + predNameDefined + "', '" + predNameNecessary + "', '" + predNameFunction1 + 
						"', '" + predNameFunction2 + "', '" + line + "'); \n");
				// out.println(line);
				// out.print("<br />\n");
				i++;
			}
			else {
				out.println("// " + line + "\n");
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
		<table class="fileViewTable" id="resultsTable"></table>
		<br />
		<br />
		<h3><a title="Back to the program files menu" href="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.FilesMgmtServlet, request, null) %>">Program Files Menu</a> &gt; 
    		View program file <%= (String) request.getAttribute("fileName") %> </h3>
    	<br /><br />
	</div>
	<script type="text/javascript">
		var table=document.getElementById("resultsTable");
		for (var i=0; i<personalizePredInfo.length; i++) {
			var row = table.insertRow(-1);
			for (var j=0; j< personalizePredInfo[i].length; j++) {
				var cell = row.insertCell(-1);
				cell.innerHTML=personalizePredInfo[i][j];
			}
		}
	</script>
</body>
</html>