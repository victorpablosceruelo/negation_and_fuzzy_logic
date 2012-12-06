<%@page import="java.util.*"%>
<%@page import="java.io.*"%>
<%@page import="java.io.InputStreamReader"%>
<%@page import="auxiliar.ServletsAuxMethodsClass"%>

<body>
    <div id="body">
		<br />
		<div class="fileViewTable">
			<div class="fileViewTableRow">
				<div class="fileViewTableCell">
		<%
			String filePath = (String) request.getAttribute("filePath");
			if ((filePath != null) && ( ! ("".equals(filePath)))) {
				BufferedReader reader = new BufferedReader(new FileReader(filePath));
				String line;
				while ((line = reader.readLine()) != null) {
       				out.println(line);
       				out.println("<br />");
				}
				reader.close();
			}
			else {
				out.println("You are not allowed to see the contents of the file " + request.getAttribute("fileName"));
			}
				
		%>
				</div>
			</div>
		</div>
		<br />
		<br />
	</div>
</body>
</html>