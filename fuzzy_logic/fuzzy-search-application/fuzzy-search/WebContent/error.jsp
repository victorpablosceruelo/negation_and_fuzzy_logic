<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">

<head>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
    <title>fuzzy search - error </title>
    <link href="css/style.css" rel="stylesheet" type="text/css" />
    <style type="text/css">
        .style1{text-align: justify;}
    </style>

</head>
<body>
    
<%@page import="java.util.*" %>

      <%
      		Enumeration<String> e1 = request.getParameterNames();
			if ( e1.hasMoreElements()) {
				%><br /><br /><br /><h1>Parameters</h1><br /><%
				while ( e1.hasMoreElements() ){
                	String paramName = (String) e1.nextElement().toString();
                	out.println("<br/><br/> " + "Parameter: " + paramName);
                
    				String[] values = request.getParameterValues(paramName);
    				for (int i=0; i<values.length; i++) {
    					out.println("<br/> " + values[i]);
//    					System.out.print(values[i]);
//    					System.out.print(", ");
    				}
				}
			}
			
      		Enumeration<String> e2 = request.getAttributeNames();
			if ( e2.hasMoreElements() ) {
				%><br /><br /><br /><h1>Attributes</h1><br /><%		
        		while( e2.hasMoreElements() ){
                	String attName = (String) e2.nextElement().toString();
                	out.println("<br/>" + attName);
                	if ((attName.equals("msg1")) || (attName.equals("msg2"))) {
                		out.print(request.getAttribute(attName));
	                }
    		    }
			}
		%>
        
    <div id="footer">
        <br /><br /><br />
    	<a href="/fuzzy-search">Back to main page</a><br />
    	<br /><br /><br />
    </div>
</body>
</html>
