
package examples;

import soot.resources.Resource;
import soot.resources.annotations.Cost;
import soot.resources.annotations.Resources;
import soot.resources.annotations.Size;

@Resources({Resource.ACCESSES_DB})
public class Join {

  /**
   * true
   *   if (types([ret/[void],this/[examples.Join]]))  {
   *        types([ret/[void],this/[examples.Join]])
   *   }   * true
   *   if (null([ret]) && any([this]))  {
   *        null([ret]) && any([this])
   *   }   * true
   *   if (this/top && ret/top)  {
   *        this/top && ret/top && size(ub,ret,1) && size(ub,this,size(this))
   *   }
   *  && cost(ub,ACCESSES_DB,8$1*1$1+4$1+1$1)
   */
  public void executeTransaction() {
    String url = "jdbc:mySubprotocol:myDataSource";
    Connection_ con;
    Statement_ stmt;

    // Initialize DB tables (not in the original program)
    DBTable suppliers = new DBTable("suppliers");
    DBTable coffees = new DBTable("coffees");

// 	try {
// 	    Class.forName("myDriver.ClassName");

// 	} catch(java.lang.ClassNotFoundException e) {
// 	    System.err.print("ClassNotFoundException: ");
// 	    System.err.println(e.getMessage());
// 	}

//	try {
    con = DriverManager_.getConnection(url, "myLogin", "myPassword");
    stmt = con.createStatement();
    ResultSet_ rs = stmt.executeQuery1("query1", suppliers, coffees);
//	    System.out.println("Supplier, Coffee:");
    executeTransactionLoop(rs, con, suppliers);
    stmt.close();
    con.close();
// 	} catch(SQLException ex) {
// 	    System.err.print("SQLException: ");
// 	    System.err.println(ex.getMessage());
// 	}	
  }

  /**
   * true
   *   if (types([ret/[void],this/[examples.Join],arg(1)/[examples.Join$ResultSet_],arg(2)/[examples.Join$Connection_],arg(3)/[examples.Join$DBTable]]))  {
   *        types([ret/[void],this/[examples.Join],arg(1)/[examples.Join$ResultSet_],arg(2)/[examples.Join$Connection_],arg(3)/[examples.Join$DBTable]])
   *   }   * true
   *   if (null([ret]) && any([this,arg(1),arg(2),arg(3)]))  {
   *        null([ret]) && any([this,arg(1),arg(2),arg(3)])
   *   }   * true
   *   if (arg(3)/top && arg(2)/top && arg(1)/top && this/top && ret/top)  {
   *        arg(3)/top && arg(2)/top && arg(1)/top && this/top && ret/top && size(ub,ret,1) && size(ub,this,size(this)) && size(ub,arg(1),size(arg(1))) && size(ub,arg(2),size(arg(2))) && size(ub,arg(3),size(arg(3)))
   *   }
   *  && cost(ub,ACCESSES_DB,size(arg(3))*size(arg(1)))
   */
  void executeTransactionLoop(ResultSet_ rs, Connection_ con, DBTable suppliers) {
    if (rs != null) {
      String supName = rs.first;
      String cofName = rs.second;
      // System.out.println("    " + supName + ", " + cofName);
      Statement_ stmt2 = con.createStatement();
      stmt2.executeQuery2("query2", suppliers);
      executeTransactionLoop(rs.next, con, suppliers);
    }
  }


  class ResultSet_ {
    public String first;
    public String second;
    public ResultSet_ next;
  }

  class Statement_ {

    @Cost(
        resources = {Resource.ACCESSES_DB},
        functions = {"+($(0,4),$(0,5))"})
    @Size("[$(0,1),$(0,2),$(0,3),$(0,4),$(0,5)]")
    //	"select SUPPLIERS.SUP_NAME, COFFEES.COF_NAME " +
    //	    "from COFFEES, SUPPLIERS " +
    //	    "where SUPPLIERS.SUP_NAME like 'Acme, Inc.' and " +
    //	    "SUPPLIERS.SUP_ID = COFFEES.SUP_ID";
    native ResultSet_ executeQuery1(String sql, Object suppliers, Object coffees);

    @Cost(
        resources = {Resource.ACCESSES_DB},
        functions = {"$(0,4)"})
    @Size("[$(0,1),$(0,2),$(0,3),$(0,4)]")
    //	"update SUPPLIERS" + "set    SUPPLIERS.PREMIUM =" + "SUPPLIERS.PREMIUM + 1" +
    //	    "where  SUPPLIERS.SUP_NAME =" + supName;
    native ResultSet_ executeQuery2(String sql, Object coffees);

    @Cost(
        resources = {Resource.ACCESSES_DB},
        functions = {"0"})
    @Size("[$(0,1),$(0,2)]")
    native void close();

  }

  class Connection_ {
    @Cost(
        resources = {Resource.ACCESSES_DB},
        functions = {"0"})
    native Statement_ createStatement(); // throws SQLException

    @Cost(
        resources = {Resource.ACCESSES_DB},
        functions = {"0"})
    native void close();
  }

  class DBTable {
    String name;

    DBTable(String name) {
      this.name = name;
    }
  }
}

class DriverManager_ {

  @Cost(
      resources = {Resource.ACCESSES_DB},
      functions = {"0"})
  native static Join.Connection_ getConnection(String url, String user, String password);
}

/*
* Copyright 2003 Sun Microsystems, Inc.  ALL RIGHTS RESERVED.
* Use of this software is authorized pursuant to the terms of the license found at
* http://developer.java.sun.com/berkeley_license.html.
*/

// package examples;

// import java.sql.*;
// public class JoinOriginal {

//     public void executeTransaction() {

// 	String url = "jdbc:mySubprotocol:myDataSource";
// 	Connection con;
// 	String query =  "select SUPPLIERS.SUP_NAME, COFFEES.COF_NAME " +
// 	    "from COFFEES, SUPPLIERS " +
// 	    "where SUPPLIERS.SUP_NAME like 'Acme, Inc.' and " +
// 	    "SUPPLIERS.SUP_ID = COFFEES.SUP_ID";

// 	Statement stmt;

// 	try {
// 	    Class.forName("myDriver.ClassName");

// 	} catch(java.lang.ClassNotFoundException e) {
// 	    System.err.print("ClassNotFoundException: ");
// 	    System.err.println(e.getMessage());
// 	}

// 	try {
// 	    con = DriverManager.getConnection(url,"myLogin", "myPassword");	
// 	    stmt = con.createStatement();								
// 	    ResultSet rs = stmt.executeQuery(query);
// 	    System.out.println("Supplier, Coffee:");
// 	    while (rs.next()) {
// 		String supName = rs.getString(1);
// 		String cofName = rs.getString(2);
// 		System.out.println("    " + supName + ", " + cofName);
// 		String query2 =  "update SUPPLIERS" +  
// 		    "set    SUPPLIERS.PREMIUM ="  + "SUPPLIERS.PREMIUM + 1" +
// 		    "where  SUPPLIERS.SUP_NAME =" +  supName;
// 		Statement stmt2 = con.createStatement();
// 		stmt2.executeQuery(query2);

// 	    }
// 	    stmt.close();
// 	    con.close();

// 	} catch(SQLException ex) {
// 	    System.err.print("SQLException: ");
// 	    System.err.println(ex.getMessage());
// 	}	
// 	return;
//     }   
//  }





