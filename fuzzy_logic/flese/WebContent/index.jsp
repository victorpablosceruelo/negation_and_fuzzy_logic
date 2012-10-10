<jsp:include page="WEB-INF/commonHead.jsp" />
<!-- JavaScript Debugging Code and more -->
<jsp:include page="WEB-INF/commonJavaScriptCode.jsp" />

<body>
<H1>FleSe : <u>Fle</u>xible <u>Se</u>arches in Databases</H1>

<%@page import="auxiliar.ServletsAuxMethodsClass"%>
<a href="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.AuthenticationServletSignout, request, null) %>">Go to the application</a>
<br />
<br />
This application has been developed by
<a href="http://babel.ls.fi.upm.es/~vpablos/">Victor Pablos Ceruelo</a>
under the supervision of 
<a href="http://babel.ls.fi.upm.es/~susana/">Susana Muñoz Hernandez</a>
as part of his research in the 
<a href="http://babel.ls.fi.upm.es/">Babel Research Group</a>.

<br /><br />
This aplication is divided in two: the web interface and the engine that processes
fuzzy queries. The web interface is a Java application 
running on a Tomcat server (behind an Apache proxy) and
the engine is a <a href="http://ciaohome.org/">Ciao Prolog</a> server
running fuzzy logic programs thanks to its packages system
and the package Rfuzzy
(see <a href="http://www.sciencedirect.com/science/article/pii/S0020025510003610">RFuzzy: Syntax, semantics and implementation details of a simple and expressive fuzzy tool over Prolog</a>).  

<br /><br />
This work is partially supported by research projects 
DESAFIOS10 (TIN2009-14599-C03-00) funded by Ministerio Ciencia e Innovación of Spain,
PROMETIDOS (P2009/TIC-1465) funded by Comunidad Autónoma de Madrid,
MCYT project TIC2003-01036
and
Research Staff Training Program (BES-2008-008320) 
funded by the Spanish Ministry of Science and Innovation.
   
It is partially supported too by the Universidad Politécnica de Madrid entities 
Departamento de Lenguajes y Sistemas Informáticos e Ingeniería de Software 
and Facultad de Informática.

</body>
</html>
