
<%@page import="auxiliar.ServletsAuxMethodsClass"%>
<h3><a href="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.FilesMgmtServlet, request, null) %>">Program Files Menu</a> &gt; Perform a query </h3>
<br />
    	
<table class="programFileChosen">
	<thead>
		<tr>
			<th colspan="2">
				Information about program file choosen 
			</th>
		</tr>
	</thead>

	<tr>
		<td>
			Program file name
		</td>
		<td>
			<script type="text/javascript">currentProgramFileName;</script>
		</td>
	</tr>
	<tr>
		<td>
			Owner
		</td>
		<td>
			<script type="text/javascript">currentProgramFileOwner;</script>
		</td>
	</tr>
</table>
<br />
