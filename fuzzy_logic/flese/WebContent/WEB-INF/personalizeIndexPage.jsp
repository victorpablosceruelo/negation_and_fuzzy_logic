<jsp:include page="commonHtmlHead.jsp" />

<%@page import="java.util.*"%>
<%@page import="java.io.*"%>
<%@page import="java.io.InputStreamReader"%>
<%@page import="auxiliar.ServletsAuxMethodsClass"%>

<body>
    <div id="body">
    	<jsp:include page="commonBodyHead.jsp" />
    	<h3><a title="Back to the program files menu" href="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.FilesMgmtServlet, request, null) %>">Program Files Menu</a> &gt; 
    		View program file <%= (String) request.getAttribute("fileName") %> </h3>
		<br />
		<br />
		<table class="fileViewTable">
			<tr><td>
		<%
			String filePath = (String) request.getAttribute("filePath");
			if ((filePath != null) && ( ! ("".equals(filePath)))) {
				BufferedReader reader = new BufferedReader(new FileReader(filePath));
				String line;
				String predPatt = "[0-9a-zA-Z \\_\\-]+[\\([0-9a-zA-Z \\_\\-]+)\\)]*";
				String regexPattern = "rfuzzy_fuzzification\\("+predPatt+"[,]{1}"+predPatt+"\\)[ ]*[:\\-]{1}[\\s\\S]*";
				while ((line = reader.readLine()) != null) {
					if (line.matches(regexPattern)) {
       					out.println(line);
       					out.print("<br />\n");
					}
				}
				reader.close();
			}
		%>
			</td></tr>
		</table>
		<br />
		<br />
		<h3><a title="Back to the program files menu" href="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.FilesMgmtServlet, request, null) %>">Program Files Menu</a> &gt; 
    		View program file <%= (String) request.getAttribute("fileName") %> </h3>
    	<br /><br />
	</div>
</body>
</html>