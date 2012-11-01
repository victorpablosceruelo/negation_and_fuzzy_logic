
<!-- beginning of commonBodyProgramQuery -->
<br />
    	
<table class="programFileChosen">
	<thead class="programFileChosen">
		<tr class="programFileChosen">
			<th colspan="2" class="programFileChosen">
				Information about program file chosen 
			</th>
		</tr>
	</thead>

	<tr>
		<td class="programFileChosen">Program file name</td>
		<td class="programFileChosen" id="currentProgramFileName"></td>
	</tr>
	<tr>
		<td class="programFileChosen">Owner</td>
		<td class="programFileChosen" id="currentProgramFileOwner"></td>
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

<!-- end of commonBodyProgramQuery -->
