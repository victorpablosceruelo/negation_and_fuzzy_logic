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
import auxiliar.ProgramAnalizedClass;
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
			ServletsAuxMethodsClass.actionOnException(ServletsAuxMethodsClass.FilesMgmtServlet, "", e, request, response, LOG);
		}
		LOG.info("--- "+doAction+" end ---");
	}

	private void personalizeServlet(String doAction, HttpServletRequest request, HttpServletResponse response) 
			throws Exception {
		// Tests if we have logged in.
		LocalUserNameClass localUserName = new LocalUserNameClass(request, response);
		// if (localUserName == null) throw new Exception("localUserName is null.");
		
		String request_op = request.getParameter("op");
		if (request_op == null) throw new Exception("op is null.");		

		String fileName = request.getParameter("fileName");
		if (fileName == null) throw new Exception("fileName is null.");
		request.setAttribute("fileName", fileName);

		String fileOwner = request.getParameter("fileOwner");
		if (fileOwner == null) throw new Exception("fileOwner is null.");
		request.setAttribute("fileOwner", fileOwner);

		FoldersUtilsClass FoldersUtilsObject = new FoldersUtilsClass();
		String filePath = FoldersUtilsObject.getCompletePathOfProgramFile(fileOwner, fileName);
		request.setAttribute("filePath", filePath);
			
		if (("edit".equals(request_op)) || ("save".equals(request_op))) {
			// In case the edit method fails.
			try {
				String fuzzification = request.getParameter("fuzzification");
				if (fuzzification == null) throw new Exception("fuzzification is null.");
				request.setAttribute("fuzzification", fuzzification);

				if ("save".equals(request_op)) {
					// In case the save method fails
					try {
						save(localUserName, fileName, fileOwner, filePath, fuzzification, request, response);
					} catch (Exception e) {
						// Forward to the jsp page.
						String additionalInfo="?op=edit&fileName="+fileName+"&fileOwner="+fileOwner+"&fuzzification="+fuzzification;
						ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.PersonalizeServlet, additionalInfo, request, response, LOG);
						// ServletsAuxMethodsClass.actionOnException(ServletsAuxMethodsClass.FilesMgmtServlet, e, request, response, LOG);
					}
				}

				// Forward to the jsp page.
				ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.PersonalizeEditPage, "", request, response, LOG);

			}
			catch (Exception e) {
				// Forward to the jsp page.
				String additionalInfo="?op=none&fileName="+fileName+"&fileOwner="+fileOwner;
				ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.PersonalizeServlet, additionalInfo, request, response, LOG);
				// ServletsAuxMethodsClass.actionOnException(ServletsAuxMethodsClass.FilesMgmtServlet, e, request, response, LOG);
			}
			
		}
		else {
			ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.PersonalizeIndexPage, "", request, response, LOG);
		}
		
	}

		
	private void save(LocalUserNameClass localUserName, String fileName, String fileOwner, String filePath, String fuzzification, 
			HttpServletRequest request, HttpServletResponse response) throws Exception {
		
		int counter=0;
		String [] [] params = null;
		String paramsDebug = "Function definition to save: ";
		
		while ( (request.getParameter("fuzzificationBars["+counter+"].fpx") != null) && 
				(request.getParameter("fuzzificationBars["+counter+"].fpy") != null)) {
			counter++;
		}
		
		if (counter>0) { 
			params = new String[counter][2];
			for (int i=0; i<counter; i++) {
				params[i][0] = request.getParameter("fuzzificationBars["+i+"].fpx");
				params[i][1] = request.getParameter("fuzzificationBars["+i+"].fpy");
				paramsDebug += "\n" + params[i][0] + " -> " + params[i][1] + " ";
			}
		}
		
		LOG.info(paramsDebug);
		
		ProgramAnalizedClass programAnalized = new ProgramAnalizedClass(filePath, null);
		programAnalized.updateProgramFile(fuzzification, localUserName, params);
		
		/* This is just to test if the send button produces errors.
		int j = 0;
		while (true) {
			j++;
		}
		*/
	}
}



/* EOF */
