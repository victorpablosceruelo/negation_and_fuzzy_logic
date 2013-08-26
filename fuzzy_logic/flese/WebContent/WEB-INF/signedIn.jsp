<jsp:include page="commonHtmlHead.jsp" />
<%@page import="java.util.Iterator"%>
<%@page import="filesAndPaths.ProgramFileInfo"%>

<%
	@SuppressWarnings("unchecked")
	Iterator<ProgramFileInfo> filesIterator = (Iterator<ProgramFileInfo>) request.getAttribute("filesIterator");
%>

<body>
   	<jsp:include page="commonBodyHead.jsp" />

	<section id="mainSection" class="">
	</section>
	<br /><br /><br /><br /><br />

	<script type="text/javascript">
		insertProgramFileSelection('mainSection');		
	</script>
</body>
</html>
