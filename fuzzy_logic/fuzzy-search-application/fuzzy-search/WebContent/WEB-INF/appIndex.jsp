<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">

<head>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
    <title>fuzzy search - social authentication  </title>
    <link href="css/style.css" rel="stylesheet" type="text/css" />
    <style type="text/css">
        .style1{text-align: justify;}
    </style>
	<script>
		function validate(obj){
			var val = obj.id.value;
			if(trimString(val).length <= 0){
				alert("Please enter OpenID URL");
				return false;
			}else{
				return true;
			}
		}
		function trimString(tempStr)
		{
		   return tempStr.replace(/^\s*|\s*$/g,"");
		}
	</script>
</head>
<body>
    <div id="main">        
        <div id="text" >
			<center>
				<h1>Fuzzy Search Social Authentication</h1 >
				<h2>Please choose a provider.</h2 >

				<a href="SocialAuthServlet?id=facebook">
				<img src=""	alt="Facebook" title="Facebook" border="0"></img>
				</a> <br /> <br />
				 
				<a href="SocialAuthServlet?id=facebook&mode=signout">Signout</a>
				<a href="SocialAuthServlet?id=facebook&mode=signin">Signin</a>
				<br /> <br /> 
				
				<a href="SocialAuthServlet?id=twitter">
				<img src="https://twitter.com/images/resources/twitter-bird-blue-on-white.png"
					alt="Twitter" title="Twitter" border="0" width="75"></img></a> 
					<br /><br /> 
				<a href="SocialAuthServlet?id=twitter&mode=signout">Signout</a>
				<a href="SocialAuthServlet?id=twitter&mode=signin">Signin</a>
				<br /> <br /> 
				
				<a href="SocialAuthServlet?id=google">
				<img src="https://ssl.gstatic.com/images/logos/google_logo_41.png"
					alt="Gmail" title="Gmail" border="0" width="75"></img></a> 
					<br /> <br />
				<a href="SocialAuthServlet?id=google&mode=signout">Signout</a>
				<a href="SocialAuthServlet?id=google&mode=signin">Signin</a>
				<br /> <br /> 
				
				<a href="SocialAuthServlet?id=yahoo">
				<img src="http://l.yimg.com/a/i/mntl/ww/events/p.gif" alt="YahooMail"
					title="YahooMail" border="0"></img></a> 
					<br /> <br /> 
				<a href="SocialAuthServlet?id=yahoo&mode=signout">Signout</a>
				<a href="SocialAuthServlet?id=yahoo&mode=signin">Signin</a>
				<br /> <br /> 
				
				<a href="SocialAuthServlet?id=hotmail"><img
					src="https://secure.shared.live.com/~Live.SiteContent.Explore/~15.40.70/header/logo_mai.png"
					alt="HotMail" title="HotMail" border="0"></img></a> <br /> <br /> <a
					href="SocialAuthServlet?id=hotmail&mode=signout">Signout</a>
				<a href="SocialAuthServlet?id=hotmail&mode=signin">Signin</a>
				<br /> <a href="SocialAuthServlet?id=linkedin"><img
					src="http://press.linkedin.com/sites/all/themes/presslinkedin/images/follow_linkedin.gif"
					alt="Linked In" title="Linked In" border="0"></img></a> <br /> <br />
				<a href="SocialAuthServlet?id=linkedin&mode=signout">Signout</a>
				<a href="SocialAuthServlet?id=linkedin&mode=signin">Signin</a>
				<br /> <a href="SocialAuthServlet?id=foursquare"><img
					src="images/foursquare.jpeg" alt="FourSquare" title="FourSquare"
					border="0"></img></a> <br /> <br /> <a
					href="SocialAuthServlet?id=foursquare&mode=signout">Signout</a>
				<a href="SocialAuthServlet?id=foursquare&mode=signin">Signin</a>
				<br /> <a href="SocialAuthServlet?id=myspace"><img
					src="images/myspace.jpeg" alt="MySpace" title="MySpace" border="0"></img></a>
				<br /> <br /> <a
					href="SocialAuthServlet?id=myspace&mode=signout">Signout</a>
				<a href="SocialAuthServlet?id=myspace&mode=signin">Signin</a>
				<br /> <a href="SocialAuthServlet?id=mendeley"><img
					src="images/mendeley.jpg" alt="Mendeley" title="Mendeley"
					border="0"></img></a> <br /> <br /> <a
					href="SocialAuthServlet?id=mendeley&mode=signout">Signout</a>
				<a href="SocialAuthServlet?id=mendeley&mode=signin">Signin</a>
				<br /> <a href="SocialAuthServlet?id=yammer"><img
					src="images/yammer.jpg" alt="Yammer" title="Yammer" border="0"></img></a>
				<br /> <br /> <a
					href="SocialAuthServlet?id=yammer&mode=signout">Signout</a>
				<a href="SocialAuthServlet?id=yammer&mode=signin">Signin</a>
				<br />

				<form action="SocialAuthServlet"
					onsubmit="return validate(this);">
					or enter OpenID url: <input type="text" value="" name="id" /> <input
						type="submit" value="Submit" />
				</form>
				<br /> <br /> <br /> <br /> <br />
			</center>
		</div>
    </div>
    
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
    
    <br /><br /><br />
    <a href="upload.jsp">Upload</a><br />
	<a href="addUser.jsp">Add User</a><br />
	<a href="index-authentication.jsp">Authentication</a><br />
	<a href="AuthServlet?openid_identifier=https://www.google.com/accounts/o8/ud">Signin</a>
    <br /><br /><br />
    
    <div id="footer">
    </div>
</body>
</html>
