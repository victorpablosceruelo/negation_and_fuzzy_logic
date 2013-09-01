<%@page import="java.util.Iterator"%> 
<%@page import="managers.FilesManagerAux"%>
<%@page import="filesAndPaths.ProgramFileInfo"%>

// This file contains only JavaScript, and you should not see this.

var mainSection = document.getElementById('mainSection');
mainSection.innerHTML = "";

addMsgToTheUser("Your session has expired. You need to sign in again.");
addMsgToTheUser("To sign in again you can press the key F5 or sign out and sign in again.");

showMsgsToTheUser();