<%
	String uploadResult = (String) request.getAttribute("uploadResult");
    if (uploadResult != null) {
    	out.println(uploadResult);
    }
    else {
    	out.println("Error: attribute uploadResult is null.");
    }
%>