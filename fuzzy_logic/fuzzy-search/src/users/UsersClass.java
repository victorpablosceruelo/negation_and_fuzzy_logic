package users;

import java.sql.*;

import auxiliar.DBConnectionClass;
//import auxiliar.StorageClass;
//import auxiliar.StorageClass.ValidPaths;


public class UsersClass {
	
	DBConnectionClass DBConnection = null;
	// StorageClass storage = new StorageClass();

	public UsersClass() throws ClassNotFoundException, SQLException {
		// Esta info convendria obtenerla de otra manera.
		DBConnection = new DBConnectionClass("org.postgresql.Driver", 
				"jdbc:postgresql://127.0.0.1:5432/", "fuzzy-search", 
				"fuzzy-search-user", "fuzzy-search-pwd");
	//	test();
	//  "org.postgresql.Driver",
	//  "postgresql"
	}

	public void createUsername (String Username, String Password) {
		
	}

}
