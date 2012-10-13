
<%@page import="auxiliar.ServletsAuxMethodsClass"%>
<h3><a href="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.FilesMgmtServlet, request, null) %>">Program Files Menu</a> &gt; Perform a query </h3>
<br />
    	
<table class="programFileChosen">
	<thead>
		<tr>
			<th colspan="2">
				Information about program file chosen 
			</th>
		</tr>
	</thead>

	<tr>
		<td>Program file name</td>
		<td id="currentProgramFileName"></td>
	</tr>
	<tr>
		<td>Owner</td>
		<td id="currentProgramFileOwner"></td>
	</tr>
</table>
<script type="text/javascript">
	if (clientSoftware == "msie") {
		document.getElementById("currentProgramFileName").innerText = currentProgramFileName;
		document.getElementById("currentProgramFileOwner").innerText = currentProgramFileOwner;
	}
	if (clientSoftware == "firefox") { 
		document.getElementById("currentProgramFileName").textContent = currentProgramFileName;
		document.getElementById("currentProgramFileOwner").textContent = currentProgramFileOwner;
	}
	if ((clientSoftware != "msie") && (clientSoftware != "firefox")) {
		document.getElementById("currentProgramFileName").innerHTML = currentProgramFileName;
		document.getElementById("currentProgramFileOwner").innerHTML = currentProgramFileOwner;		
	}
		
	
</script>
<br />
