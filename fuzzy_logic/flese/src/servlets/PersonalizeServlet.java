package servlets;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import auxiliar.FoldersUtilsClass;
import auxiliar.LocalUserNameClass;
import auxiliar.ServletsAuxMethodsClass;

/**
 * Servlet implementation class PersonalizeServlet
 */
@WebServlet("/PersonalizeServlet")
public class PersonalizeServlet extends HttpServlet {
	private static final long serialVersionUID = 1L;
	final Log LOG = LogFactory.getLog(PersonalizeServlet.class);
	
	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		doGetAndDoPost("doGet", request, response);
	}

	/**
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		doGetAndDoPost("doPost", request, response);
	}
	
	private void doGetAndDoPost(String doAction, HttpServletRequest request, HttpServletResponse response) 
			throws ServletException, IOException {
		LOG.info("--- "+doAction+" invocation ---");
		try {
			personalizeServlet(doAction, request, response);
		} catch (Exception e) {
			ServletsAuxMethodsClass.actionOnException(ServletsAuxMethodsClass.FilesMgmtServlet, e, request, response, LOG);
		}
		LOG.info("--- "+doAction+" end ---");
	}

	private void personalizeServlet(String doAction, HttpServletRequest request, HttpServletResponse response) 
			throws Exception {
		// Tests if we have logged in.
		LocalUserNameClass localUserName = new LocalUserNameClass(request, response);
		
		String request_op = request.getParameter("op");
		LOG.info("op: " + request_op);
		if (request_op != null) {
			try {
				if ("edit".equals(request_op)) {
					edit(doAction, localUserName, request, response);
				}
				if ("save".equals(request_op)) {
					save(doAction, localUserName, request, response);
				}

				if ((! "edit".equals(request_op)) && (! ("save".equals(request_op)))) {
					throw new Exception("Unknown operation.");
				}
			} catch (Exception e) {
				ServletsAuxMethodsClass.actionOnException(ServletsAuxMethodsClass.PersonalizeServlet, e, request, response, LOG);
			}
		}

		if ((request_op == null) || (! ("edit".equals(request_op)))) {
			list_functions_to_edit(doAction, localUserName, request, response);
		}
	}

	private void list_functions_to_edit(String doAction, LocalUserNameClass localUserName, HttpServletRequest request, HttpServletResponse response) 
			throws Exception {
		if (localUserName == null) throw new Exception("localUserName is null.");
		String fileName = request.getParameter("fileName");
		if (fileName == null) throw new Exception("fileName is null.");
		request.setAttribute("fileName", fileName);
		String fileOwner = request.getParameter("fileOwner");
		if (fileOwner == null) throw new Exception("fileOwner is null.");
		request.setAttribute("fileOwner", fileOwner);

		FoldersUtilsClass FoldersUtilsObject = new FoldersUtilsClass();
		String filePath = FoldersUtilsObject.getCompletePathOfProgramFile(fileOwner, fileName);
		request.setAttribute("filePath", filePath);
		
		ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.PersonalizeIndexPage, request, response, LOG);
	}
	
	private void edit(String doAction, LocalUserNameClass localUserName, HttpServletRequest request, HttpServletResponse response) 
			throws Exception {

		if (localUserName == null) throw new Exception("localUserName is null.");
		String fileName = request.getParameter("fileName");
		if (fileName == null) throw new Exception("fileName is null.");
		request.setAttribute("fileName", fileName);
		String fileOwner = request.getParameter("fileOwner");
		if (fileOwner == null) throw new Exception("fileOwner is null.");
		request.setAttribute("fileOwner", fileOwner);
		String fuzzification = request.getParameter("fuzzification");
		if (fuzzification == null) throw new Exception("fuzzification is null.");
		request.setAttribute("fuzzification", fuzzification);
		
		FoldersUtilsClass FoldersUtilsObject = new FoldersUtilsClass();
		String filePath = FoldersUtilsObject.getCompletePathOfProgramFile(fileOwner, fileName);
		request.setAttribute("filePath", filePath);
		
		// Forward to the jsp page.
		ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.PersonalizeEditPage, request, response, LOG);
	}
	
	private void save(String doAction, LocalUserNameClass localUserName, HttpServletRequest request, HttpServletResponse response) {

	}
}
