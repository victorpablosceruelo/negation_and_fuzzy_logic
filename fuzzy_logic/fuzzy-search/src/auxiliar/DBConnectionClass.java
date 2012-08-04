package auxiliar;

import java.sql.Connection;
import java.sql.SQLException;
// import org.postgresql.Driver;
// import org.postgresql.*;
// import java.sql.DriverManager;

public class DBConnectionClass {

	static private Connection connection = null;

	public DBConnectionClass (String DB_Driver, String DB_path, String DB_name, String DB_user, String DB_pwd) 
			throws ClassNotFoundException, SQLException{
		
		if (connection != null) {
			System.out.println("No need to PostgreSQL JDBC Re-Connect. ");
			return;
		}
		else {
			System.out.println("-------- PostgreSQL JDBC Connecting ------------");
			// register_driver(DB_Driver);
			
			try {
				connection = new org.postgresql.jdbc4.Jdbc4Connection("localhost", 0, DB_pwd, DB_pwd, null, DB_pwd);
				// DriverManager.getConnection(DB_path + DB_name, DB_user, DB_pwd);
				// connection = DriverManager.getConnection(
				//				"jdbc:postgresql://127.0.0.1:5432/testdb", "mkyong", "123456"); 
			} catch (SQLException e) {
				System.out.println("Connection Failed! Check output console");
				e.printStackTrace();
				throw e;
			}

			if (connection == null) {
				throw new SQLException("Unknown problem when trying to connect to database.");
			}	

			System.out.println("-------- PostgreSQL JDBC Connection established ------------");
		}
	}
}

/*
	private void register_driver(String DB_Driver) throws ClassNotFoundException, SQLException { 
		System.out.println("Registering PostgreSQL JDBC Driver"); 
		try {
			System.out.println("Driver: " + DB_Driver);
			java.sql.DriverManager.registerDriver(new org.postgresql.jdbc4.Jdbc4Connection());
//		org.postgresql.Driver
// 		java.sql.DriverManager.registerDriver (new org.gjt.mm.mysql.Driver());
//			Class.forName(DB_Driver);
		} catch (SQLException e) {
			System.out.println("Where is your PostgreSQL JDBC Driver? "
					+ "Include in your library path!");
			e.printStackTrace();
			throw e;			
		}

		System.out.println("PostgreSQL JDBC Driver Registered!");
	}
	
}

/*
 catch (ClassNotFoundException e) {
 				System.out.println("Where is your PostgreSQL JDBC Driver? "
						+ "Include in your library path!");
				e.printStackTrace();
				throw e;
			}
*/ 

/*
Connection conn = null;
SQLite.Database db = null;
try {
  Class.forName("SQLite.JDBCDriver").newInstance();
  conn = DriverManager.getConnection("jdbc:sqlite:/blabla");
  java.lang.reflect.Method m =
    conn.getClass().getMethod("getSQLiteDatabase", null);
  db = (SQLite.Database) m.invoke(conn, null);
} catch (Exception e) {
}
*/

/*
private void test () { 
	{
		Connection c = null;
		try {
			Class.forName("SQLite.JDBCDriver");
			// Note: /test.db is the test.db in the *current* working directory
			
			String database = "jdbc:sqlite:" + storage.getPath(ValidPaths.usersDB);
			c = DriverManager.getConnection(database,"","");
			c.setAutoCommit(false);

			Statement st = c.createStatement();
			int rc = st.executeUpdate( "INSERT INTO x(b) VALUES('qwer')" );
			System.out.println( "insert returns " + rc );

			ResultSet rs = st.executeQuery( "SELECT * FROM x" );
			while ( rs.next() ) {
				int i = rs.getInt(1);
				String  s = rs.getString(2);
				System.out.println( "i=" + i + ", s=" + s );
			}
			rs.close();
			st.close();

			c.commit();
			c.close();
		} catch ( Exception e ) {
			System.err.println( e.getClass().getName() + ": " + e.getMessage() );
			System.exit(1);
			try {
				if ( c != null && ! c.isClosed() ) {
					c.rollback();
					c.close();
				}
			} catch ( SQLException sql ) { 
				// ignore
			}
		}
	}
*/



// EOF