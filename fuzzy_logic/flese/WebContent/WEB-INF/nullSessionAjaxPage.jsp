<%@page import="java.util.Iterator"%> 
<%@page import="filesAndPaths.FilesMgmt"%>
<%@page import="filesAndPaths.FileInfoClass"%>

// This file contains only JavaScript, and you should not see this.

var mainSection = document.getElementById('mainSection');
mainSection.innerHTML = "";

addMsgToTheUser("Your session has expired. You need to sign in again.");
addMsgToTheUser("To sign in again you can press the key F5 or sign out and sign in again.");

showMsgsToTheUser();