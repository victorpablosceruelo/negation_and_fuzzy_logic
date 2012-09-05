/*---------------------------------------------------------------
 * example0.java
 * Java to ciao interface example with no graphical interface. 
 */
import CiaoJava.*;
// import java.awt.*;
// import java.awt.event.*;

public class javaLoader {

    // Prolog process.
    private static PLConnection plServer = null;

    public static void main(String argv[]) {
	String argsPlConnection [] = new String[1];
	String moduleToLoad = null;
	try {
	    if (argv.length == 0)
		plServer = new PLConnection();
	    else if (argv.length == 1)
		plServer = new PLConnection(argv);
	    else if (argv.length == 2) {
		argsPlConnection [0] = argv[0];
		plServer = new PLConnection(argsPlConnection);
		moduleToLoad = argv[1];
	    }
	} catch (Exception e) {
	    System.err.println("Problems starting java server: " + e);
	    e.printStackTrace();
	    System.exit(1);
	}

	PLGoal goal = null;

	// Load needed modules.
	try {
	    // "library(lists)" // for lists:append/3
	    if (moduleToLoad != null) {
		// String realModuleToLoad = ".(" + moduleToLoad + ")";
		// System.out.println("Loading module: " + realModuleToLoad);
		// goal.useModule(realModuleToLoad); 
		PLStructure query1 = new PLStructure("use_module", new PLTerm[]{new PLAtom(moduleToLoad)}); 
		goal = new PLGoal(plServer,query1); 
		goalQuery(goal);
		getTheAnswers(goal, query1);

	    }
	    else {
		System.out.println("No module to load ");
	    }
	} catch (Exception e) {
	    System.err.println("Problems loading module: " + e);
	    System.exit(1);
	}

	// The second goal.
	PLVariable predType = new PLVariable();
	PLVariable predName = new PLVariable();
	PLVariable predArity = new PLVariable();
	PLTerm[] args = {predType, predName, predArity};
	PLStructure query2 = new PLStructure("rfuzzy_introspection", args);

	goal = new PLGoal(plServer,query2); 
	goalQuery(goal);
	getTheAnswers(goal, query2);

	try {
	    plServer.stop();
	    System.exit(0);
	} catch (Exception e) {
	    System.err.println("Problems stopping Prolog server: " + e);
	    System.exit(1);
	}

    }

    private static void getTheAnswers (PLGoal goal, PLStructure query) {
	if (goal != null) {
	    // Get the answers.
	    System.out.println("Getting the answers ... ");
	    System.out.println("Input goal: " + goal);
	    System.out.println("Input query: " + query);

	    try {	    
		PLTerm queryAnswer = null;
		do {
		    queryAnswer = null;
		    queryAnswer = goal.nextSolution();
		    System.out.println("Solution: ");
		    System.out.println("Solution goal: " + goal);
		    System.out.println("Solution query: " + query);
		    System.out.println("Query Answer: " + queryAnswer);
		} while (queryAnswer != null); 
		System.out.println("There are no more solutions");
		System.out.println(" ");
	    } catch (Exception e) {
		System.err.println("Problems getting the answers: " + e);
		System.exit(1);
	    }	
	}
	else {
	    System.out.println("Getting the answers ... ERROR: goal is null. ");
	}
    }

    private static void goalQuery (PLGoal goal) {
	System.out.println("Query: " + goal);
	// Run the query.
	try {
	    System.out.println("goal.query ... ");
	    goal.query();
	} catch (Exception e) {
	    System.err.println("Problems launching goal: " + e);
	    System.exit(1);
	}
    }
}
/*---------------------------------------------------------------*/

/* -- Does it really work ???
  if (goal.execute()) {
  System.out.println("useModule: " + useModule + " ---> ok");
  }
  else {
  System.out.println("useModule: " + useModule + " ---> fail");
  }
*/