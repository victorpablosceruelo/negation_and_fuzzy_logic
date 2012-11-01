<jsp:include page="commonHtmlHead.jsp" />

<%@page import="java.util.*"%>
<%@page import="java.io.*"%>
<%@page import="java.io.InputStreamReader"%>
<%@page import="auxiliar.ServletsAuxMethodsClass"%>

<body>
    <div id="body">
    	<jsp:include page="commonBodyHead.jsp" />
    	<h3><a href="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.FilesMgmtServlet, request, null) %>">Program Files Menu</a> &gt; 
    		View program file <%= (String) request.getAttribute("fileName") %> </h3>
		<br />
		<hr>
		<br />
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
		<br />
		<hr>
		<br />
		<h3><a href="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.FilesMgmtServlet, request, null) %>">Program Files Menu</a> &gt; 
    		View program file <%= (String) request.getAttribute("fileName") %> </h3>
    	<br /><br />
	</div>
</body>
</html>