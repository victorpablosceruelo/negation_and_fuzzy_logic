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
		<div id="personalizationTableDiv"></div><table id="resultsTable" class="personalizationTable"></table>
		<br />
		<br />
    	<h3><a title="Back to the program files menu" href="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.FilesMgmtServlet, request, null) %>">Program Files Menu</a> &gt; 
    		Personalize program file <%= (String) request.getAttribute("fileName") %> 
    	</h3>
    	<br /><br />
	</div>
	<script type="text/javascript">
	
		var personalizeServletEditAction = "<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.PersonalizeServletEditAction, request, null) %>";
		var fileName = "<%= (String) request.getAttribute("fileName") %>";
		var fileOwner = "<%= (String) request.getAttribute("fileOwner") %>";
		
		if (personalizePredInfo.length > 0) {
			var divContainer = document.getElementById("personalizationTableDiv");
			var table = document.createElement('div');
			table.id = "personalizationTable";
			table.className = "personalizationTable";
			divContainer.appendChild(table);
		
			var row = document.createElement('div');
			row.className = "personalizationTable";
			table.appendChild(row);
			
			var cell = null;
			cell = document.createElement('div');
			cell.className = "personalizationTable";
			cell.innerHTML = "Fuzzification";
			row.appendChild(cell);
			
			cell = document.createElement('div');
			cell.className = "personalizationTable";
			cell.innerHTML = "depends on the values of";
			row.appendChild(cell);
			
			cell = document.createElement('div');
			cell.className = "personalizationGraphicsInTable";
			cell.innerHTML = "applying function";
			row.appendChild(cell);
			
			for (var i=0; i<personalizePredInfo.length; i++) {				
				if (personalizePredInfo[i].length >= 3){

					// row.
					row = document.createElement('div');
					row.className = "personalizationTable";
					table.appendChild(row);

					cell = document.createElement('div');
					cell.className = "personalizationTable";
					cell.innerHTML="<a title='Personalize fuzzification "+personalizePredInfo[i][0]+"' href='" + personalizeServletEditAction + "&fileName="+fileName+"&fileOwner="+fileOwner+"&fuzzification="+personalizePredInfo[i][0]+"'>"+personalizePredInfo[i][0]+"</a>";
					row.appendChild(cell);

					cell = document.createElement('div');
					cell.className = "personalizationTable";
					cell.innerHTML=personalizePredInfo[i][1];
					row.appendChild(cell);

					cell = document.createElement('div');
					cell.className = "personalizationGraphicsInTable";
					cell.id = 'chartDiv_' + i;
					row.appendChild(cell);
					
					// cell.innerHTML=personalizePredInfo[i][2];

					drawChart(cell.id, i);
				}
			}
		}
	</script>
</body>
</html>