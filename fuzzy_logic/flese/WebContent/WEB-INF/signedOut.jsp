<jsp:include page="commonHtmlHead.jsp" />

<%@page import="constants.KConstants"%>
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
			
		<h3>
			To access to FleSe you need an account in a Social Authentication Provider.
			<br />
			Please choose one of the following or provide an OpenId url.
		</h3>

		<table class="providersIcons">
			<tr class="providersIcons">
				<td class="providersIcons">
					<a id="facebookLink" href="">
					<img src="images/facebook_icon.png" alt="Facebook" title="Facebook" border="0" width="50"></img>
					</a>
				</td>
				<td class="providersIcons">			
					<a id="twitterLink" href="">
					<img src="images/twitter_icon.png" alt="Twitter" title="Twitter" border="0" width="50"></img></a> 
				</td> 
				<td class="providersIcons">
					<a id="gmailLink" href="">
					<img src="images/gmail_icon.png" alt="Gmail" title="Gmail" border="0" width="50"></img></a> 
				</td>
			</tr>
			<tr class="providersIcons">
				<td class="providersIcons">
					<a id="yahooLink" href="">
					<img src="images/yahoomail_icon.png" alt="YahooMail" title="YahooMail" border="0" width="50"></img></a> 
				</td> 
				<td class="providersIcons">
					<a id="hotmailLink" href="">
					<img src="images/hotmail_icon.jpeg" alt="HotMail" title="HotMail" border="0" width="50"></img></a> 
				</td> 
				<td class="providersIcons">
					<a id="linkedinLink" href="">
					<img src="images/linkedin_icon.gif" alt="Linked In" title="Linked In" border="0" width="50"></img></a> 
				</td>
			</tr>
			<tr class="providersIcons">
				<td class="providersIcons">
					<a id="foursquareLink" href="">
					<img src="images/foursquare_icon.jpeg" alt="FourSquare" title="FourSquare" border="0" width="50"></img></a> 
				</td>
				<td class="providersIcons">
					<a id="myspaceLink" href="">
					<img src="images/myspace_icon.jpeg" alt="MySpace" title="MySpace" border="0" width="50"></img></a>
				</td> 
				<td class="providersIcons">
					<a id="mendeleyLink" href="">
					<img src="images/mendeley_icon.jpg" alt="Mendeley" title="Mendeley" border="0" width="50"></img>
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
		<h3>or enter OpenID url: </h3>	
		<form action="SocialAuthServlet" onsubmit="return validate(this);">
			<input type="text" value="" name="id" /> 
			<input type="submit" value="Submit" />
		</form>
		<br /> <br /> <br /> <br /> <br />

    </div>
    
    <div id="footer">
    </div>

	<script type="text/javascript">
		setupHref ('facebookLink', "<%= KConstants.Pages.SignInRequest.getUrl() %>" + "&id=facebook");
		setupHref ('twitterLink', "<%= KConstants.Pages.SignInRequest.getUrl() %>" + "&id=twitter");
		setupHref ('gmailLink', "<%= KConstants.Pages.SignInRequest.getUrl() %>" + "&id=google");
		setupHref ('yahooLink', "<%= KConstants.Pages.SignInRequest.getUrl() %>" + "&id=yahoo");
		setupHref ('hotmailLink', "<%= KConstants.Pages.SignInRequest.getUrl() %>" + "&id=hotmail");
		setupHref ('linkedinLink', "<%= KConstants.Pages.SignInRequest.getUrl() %>" + "&id=linkedin");
		setupHref ('foursquareLink', "<%= KConstants.Pages.SignInRequest.getUrl() %>" + "&id=foursquare");
		setupHref ('myspaceLink', "<%= KConstants.Pages.SignInRequest.getUrl() %>" + "&id=myspace");
		setupHref ('mendeleyLink', "<%= KConstants.Pages.SignInRequest.getUrl() %>" + "&id=mendeley");
		showMsgsToTheUser();
	</script>
	
    
    
</body>
</html>
