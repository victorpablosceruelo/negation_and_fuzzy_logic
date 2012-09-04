<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
<title>Insert title here</title>
</head>
<body>

<%@page import="java.util.*"%>
<%@page import="java.io.*"%>
<%
	String filePath = (String) request.getAttribute("filePath");
	if ((filePath != null) && ( ! ("".equals(filePath)))) {
      File sourceFile = new File (filePath);
      FileReader fr = null;
      try {
         fr = new FileReader (sourceFile);
         int inChar;

         while ( (inChar = fr.read()) != -1 ) {
        	 out.print(inChar);
            // System.out.printf ("%c", inChar);
         }
      } catch (IOException e) {
         System.err.printf ("Failure while reading %s: %s\n",
                            filePath, e.getMessage());
         e.printStackTrace ();
      } finally {
         try {
            if (fr != null) { fr.close (); }
         } catch (IOException e) {
            System.err.printf ("Error closing file reader: %s\n",
                               e.getMessage());
            e.printStackTrace ();
         }
      }
	}
%>
</body>
</html>