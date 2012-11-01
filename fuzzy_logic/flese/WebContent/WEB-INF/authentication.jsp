<jsp:include page="commonHtmlHead.jsp" />

<%@page import="auxiliar.ServletsAuxMethodsClass"%>

	<script type="text/javascript">
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
	
<body>
    <div id="body">
    	<jsp:include page="commonBodyHead.jsp" />
			
		<h2>
			To access to FleSe you need an account in a Social Authentication Provider.
			<br />
			Please choose one of the following or provide an OpenId url.
		</h2 >

		<table class="providersIcons">
			<tr class="providersIcons">
				<td class="providersIcons">
					<a href="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.AuthenticationServletSignin, request, null) %>&id=facebook">
					<img src="facebook_icon.png" alt="Facebook" title="Facebook" border="0" width="50"></img>
					</a>
				</td>
				<td class="providersIcons">			
					<a href="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.AuthenticationServletSignin, request, null) %>&id=twitter">
					<img src="twitter_icon.png" alt="Twitter" title="Twitter" border="0" width="50"></img></a> 
				</td> 
				<td class="providersIcons">
					<a href="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.AuthenticationServletSignin, request, null) %>&id=google">
					<img src="gmail_icon.png" alt="Gmail" title="Gmail" border="0" width="50"></img></a> 
				</td>
			</tr>
			<tr class="providersIcons">
				<td class="providersIcons">
					<a href="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.AuthenticationServletSignin, request, null) %>&id=yahoo">
					<img src="yahoomail_icon.png" alt="YahooMail" title="YahooMail" border="0" width="50"></img></a> 
				</td> 
				<td class="providersIcons">
					<a href="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.AuthenticationServletSignin, request, null) %>&id=hotmail">
					<img src="hotmail_icon.jpeg" alt="HotMail" title="HotMail" border="0" width="50"></img></a> 
				</td> 
				<td class="providersIcons">
					<a href="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.AuthenticationServletSignin, request, null) %>&id=linkedin">
					<img src="linkedin_icon.gif" alt="Linked In" title="Linked In" border="0" width="50"></img></a> 
				</td>
			</tr>
			<tr class="providersIcons">
				<td class="providersIcons">
					<a href="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.AuthenticationServletSignin, request, null) %>&id=foursquare">
					<img src="foursquare_icon.jpeg" alt="FourSquare" title="FourSquare" border="0" width="50"></img></a> 
				</td>
				<td class="providersIcons">
					<a href="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.AuthenticationServletSignin, request, null) %>&id=myspace">
					<img src="myspace_icon.jpeg" alt="MySpace" title="MySpace" border="0" width="50"></img></a>
				</td> 
				<td class="providersIcons">
					<a href="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.AuthenticationServletSignin, request, null) %>&id=mendeley">
					<img src="mendeley_icon.jpg" alt="Mendeley" title="Mendeley" border="0" width="50"></img>
					</a> 
				</td>
			</tr>
		<!--  Yammer is a Private Social Network for Your Company					
		<a href="SocialAuthServlet?id=yammer&op=signin"><img
			src="images/yammer.jpg" alt="Yammer" title="Yammer" border="0"></img></a>
		<br /> <br /> 
		 -->
		</table>
			
		<br />
		<h2>or enter OpenID url: </h2>	
		<form action="SocialAuthServlet" onsubmit="return validate(this);">
			<input type="text" value="" name="id" /> 
			<input type="submit" value="Submit" />
		</form>
		<br /> <br /> <br /> <br /> <br />

    </div>
    
    <div id="footer">
    </div>
</body>
</html>
