package soot.resources;

import soot.BlockMethod;
import soot.jimple.InvokeStmt;

public interface ResourceFunction {

  String INFINITY = "inf";

  public String getCost(BlockMethod blockMethod);

  public String getCost(InvokeStmt invokeStmt);

}
