package servlets;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;


import auxiliar.ServletsAuxMethodsClass;

/**
 * Servlet implementation class SearchServlet
 */
@WebServlet("/DataBasesMenuServlet")
public class DataBasesMenuServlet extends HttpServlet {
	private static final long serialVersionUID = 1L;
	final Log LOG = LogFactory.getLog(DataBasesMenuServlet.class);

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) 
			throws ServletException, IOException {
		LOG.info("--- doGet invocation ---");
		doGetAndDoPost(request, response);
		LOG.info("--- doGet end ---");
	}

	/**
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		LOG.info("--- doPost invocation ---");
		doGetAndDoPost(request, response);
		LOG.info("--- doPost end ---");	
	}
	
	private void doGetAndDoPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, java.io.IOException {
		try {
			dataBasesMenu(request, response);
		} catch (Exception e) {
			LOG.error("Exception thrown: ");
			LOG.error(e);
			e.printStackTrace();
			ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.AuthenticationServletSignout, request, response, LOG);
		}
	}

	private void dataBasesMenu(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		if (ServletsAuxMethodsClass.clientSessionIsAuthenticated(request, response, LOG)) {
			ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.DataBasesMenu_Page, request, response, LOG);
		}
	}
	
	/*
	private void performQuery(HttpSession session, HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		ServletsAuxMethodsClass.goToDataBaseQuery(request, response, LOG);
	}
	*/
	

}
