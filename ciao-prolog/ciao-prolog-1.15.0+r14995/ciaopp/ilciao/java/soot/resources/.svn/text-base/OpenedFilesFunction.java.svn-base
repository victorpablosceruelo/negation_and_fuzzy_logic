package soot.resources;

import soot.*;
import soot.jimple.InvokeStmt;

public class OpenedFilesFunction extends GenericResourceFunction {

  OpenedFilesFunction() {
    super(Resource.OPENED_FILES);
  }

  @Override
  public String getCost(InvokeStmt invokeStmt) {
    BlockMethod blockMethod = (BlockMethod) invokeStmt.getInvokeExpr().getMethod();
    if ( (blockMethod.getBlockMethodName().contains("FileInputStream") && 
          blockMethod.getBlockMethodName().contains("init")) ||
         (blockMethod.getBlockMethodName().contains("FileOutputStream") && 
          blockMethod.getBlockMethodName().contains("init")) )  {
	return cost(1);
    }
    else
	return cost(0);
	    
  }
}
