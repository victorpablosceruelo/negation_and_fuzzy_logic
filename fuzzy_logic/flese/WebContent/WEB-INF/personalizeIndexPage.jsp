<jsp:include page="commonHtmlHead.jsp" />

<%@page import="java.util.*"%>
<%@page import="java.io.*"%>
<%@page import="java.io.InputStreamReader"%>
<%@page import="auxiliar.ServletsAuxMethodsClass"%>

<jsp:include page="commonPersonalization.jsp" />

<body>
    <div id="body">
    	<jsp:include page="commonBodyHead.jsp" />
    	<h3><a title="Back to the program files menu" href="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.FilesMgmtServlet, request, null) %>">Program Files Menu</a> &gt; 
    		Personalize program file <%= (String) request.getAttribute("fileName") %> 
    	</h3>
		<br />
		<br />
		<div id="fuzzificationDefsTableDiv"></div><table id="resultsTable" class="fuzzificationDefsTable"></table>
		<br />
		<br />
    	<h3><a title="Back to the program files menu" href="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.FilesMgmtServlet, request, null) %>">Program Files Menu</a> &gt; 
    		Personalize program file <%= (String) request.getAttribute("fileName") %> 
    	</h3>
    	<br /><br />
	</div>
	<script type="text/javascript">
			
		if (fuzzificationDefs.length > 0) {
			var divContainer = document.getElementById("fuzzificationDefsTableDiv");
			var table = null;
			var row = null;
			var cell = null;
			
			for (var i=0; i<fuzzificationDefs.length; i++) {				
				if (fuzzificationDefs[i] != null){

					table = document.createElement('div');
					table.id = "fuzzificationDefsTable";
					table.className = "fuzzificationDefsTable";
					divContainer.appendChild(table);
					
					cell = document.createElement('div');
					cell.className = "fuzzificationDefsTableCaption";
					cell.innerHTML="Graphical representation of the fuzzification " + 
									"<a title='Personalize fuzzification "+fuzzificationDefs[i].predDefined+"' href='" + personalizeServletEditAction + "&fileName="+fileName+"&fileOwner="+fileOwner+"&fuzzification="+fuzzificationDefs[i].predDefined+"'>"+fuzzificationDefs[i].predDefined+"</a>."+
									" The input values fuzzified are obtained using the getter " + fuzzificationDefs[i].predNecessary;
					table.appendChild(cell);

					// row.
					row = document.createElement('div');
					row.className = "fuzzificationDefsTableRow";
					table.appendChild(row);
					
					cell = document.createElement('div');
					cell.className = "fuzzificationDefsTableCellForGraphic";
					cell.id = 'chartDiv_' + i;
					row.appendChild(cell);
					
					// cell.innerHTML=fuzzificationDefs[i][2];

					drawChart(cell.id, i);
				}
			}
		}
	</script>
</body>
</html>