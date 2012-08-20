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
        
        	<jsp:include page="showErrors.jsp" />
			
			<center>
				<h1>Fuzzy Search Social Authentication</h1 >
				<h2>Please choose a provider to sign in.</h2 >

				<a href="SocialAuthServlet?id=facebook&mode=signin">
				<img src="http://upload.wikimedia.org/wikipedia/commons/0/06/Facebook.svg" alt="Facebook" title="Facebook" border="0" width="100"></img>
				</a> <br /> <br />
								
				<a href="SocialAuthServlet?id=twitter&mode=signin">
				<img src="https://twitter.com/images/resources/twitter-bird-blue-on-white.png" alt="Twitter" title="Twitter" border="0" width="75"></img></a> 
					<br /><br /> 
				
				<a href="SocialAuthServlet?id=google&mode=signin">
				<img src="https://ssl.gstatic.com/images/logos/google_logo_41.png" alt="Gmail" title="Gmail" border="0" width="100"></img></a> 
					<br /> <br />
				
				<a href="SocialAuthServlet?id=yahoo&mode=signin">
				<img src="http://upload.wikimedia.org/wikipedia/commons/c/c6/YahooMailLogo.svg" alt="YahooMail" title="YahooMail" border="0" width="100"></img></a> 
					<br /> <br /> 
				
				<a href="SocialAuthServlet?id=hotmail&mode=signin">
				<img src="https://secure.shared.live.com/~Live.SiteContent.Explore/~15.40.70/header/logo_mai.png" alt="HotMail" title="HotMail" border="0" width="100"></img></a> 
					<br /> <br /> 
				
				<a href="SocialAuthServlet?id=linkedin&mode=signin">
				<img src="http://press.linkedin.com/sites/all/themes/presslinkedin/images/follow_linkedin.gif" alt="Linked In" title="Linked In" border="0" width="50"></img></a> 
					<br /> <br />
				
				<br /> <a href="SocialAuthServlet?id=foursquare&mode=signin">
				<img src="images/foursquare.jpeg" alt="FourSquare" title="FourSquare" border="0" width="75"></img></a> 
					<br /> <br />
					 
				<a href="SocialAuthServlet?id=myspace&mode=signin">
				<img src="images/myspace.jpeg" alt="MySpace" title="MySpace" border="0"></img></a>
				<br /> <br /> 
				
				<a href="SocialAuthServlet?id=mendeley&mode=signin">
				<img src="images/mendeley.jpg" alt="Mendeley" title="Mendeley"
					border="0"></img></a> <br /> <br /> 

				<!--  Yammer is a Private Social Network for Your Company					
				<a href="SocialAuthServlet?id=yammer&mode=signin"><img
					src="images/yammer.jpg" alt="Yammer" title="Yammer" border="0"></img></a>
				<br /> <br /> 
				 -->

				<form action="SocialAuthServlet"
					onsubmit="return validate(this);">
					or enter OpenID url: <input type="text" value="" name="id" /> <input
						type="submit" value="Submit" />
				</form>
				<br /> <br /> <br /> <br /> <br />
			</center>
		</div>
    </div>
    
    <div id="footer">
    </div>
</body>
</html>
