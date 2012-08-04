package users;

import java.io.IOException;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
// import java.sql.SQLException;

/**
 * Servlet implementation class UsersServlet
 */
@WebServlet("/UsersServlet")
public class UsersServlet extends HttpServlet {
	private static final long serialVersionUID = 1L;
    
	UserClass users = null;
	
    /**
     * @see HttpServlet#HttpServlet()
     */
    public UsersServlet() {
        super();
    }

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		// TODO Auto-generated method stub
		System.out.println("Redirecting to addUser.jsp ");
		response.encodeRedirectURL("addUser.jsp");
	}

	/**
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		// TODO Auto-generated method stub
		Boolean error_en_conexion = false;
		try {
			users = new UserClass();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			error_en_conexion = true;
		} 
		
		if (error_en_conexion) {
			System.out.println("Redirecting to databaseError.jsp ");
			response.encodeRedirectURL("databaseError.jsp");
		}
		else {
			System.out.println("Creating new user ...");
			String username = request.getParameter("username");
			String password = request.getParameter("password");
			users.createUsername(username, password);
		}
	}

}
