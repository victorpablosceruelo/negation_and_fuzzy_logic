<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
<title>Add user to the application</title>
</head>
<body>

<FORM ENCTYPE='multipart/form-data' method='POST' action="UsersServlet">
<div class="email-div">
  <label for="Email"><strong class="email-label">Username</strong></label>
  <input type="text" name="Email" id="Email" value="">
  <!-- spellcheck="false" -->
</div>
<div class="passwd-div">
  <label for="Passwd"><strong class="passwd-label">Password</strong></label>
  <input type="password" name="Passwd" id="Passwd">
</div>
<INPUT TYPE='submit' VALUE='submit'>
</FORM>

</body>
</html>