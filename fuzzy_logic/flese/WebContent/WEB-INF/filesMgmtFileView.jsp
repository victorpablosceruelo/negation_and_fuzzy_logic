<jsp:include page="commonHead.jsp" />
<!-- JavaScript Debugging Code and more -->
<jsp:include page="commonJavaScriptCode.jsp" />

<%@page import="java.util.*"%>
<%@page import="java.io.*"%>
<%@page import="java.io.InputStreamReader"%>
<%@page import="auxiliar.ServletsAuxMethodsClass"%>

<body>
    <div id="body">
    	<jsp:include page="commonBody.jsp" />
    	<h3><a href="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.FilesMgmtServlet, request, null) %>">Program Files Menu</a> &gt; 
    		View program file <%= (String) request.getAttribute("fileName") %> </h3>
		<br /><br />
		<%
			String filePath = (String) request.getAttribute("filePath");
			if ((filePath != null) && ( ! ("".equals(filePath)))) {
				BufferedReader reader = new BufferedReader(new FileReader(filePath));
				String line;
				while ((line = reader.readLine()) != null) {
       				out.println(line);
       				out.print("<br />\n");
				}
				reader.close();
			}
		%>
	</div>
</body>
</html>