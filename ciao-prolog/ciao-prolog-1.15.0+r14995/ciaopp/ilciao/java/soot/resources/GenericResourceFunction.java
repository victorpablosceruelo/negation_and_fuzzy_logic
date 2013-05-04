package soot.resources;

import soot.BlockMethod;
import soot.jimple.InvokeStmt;
import soot.resources.annotations.Cost;
import soot.util.SootUtil;

public class GenericResourceFunction implements ResourceFunction {

  private Resource resource;

  GenericResourceFunction(Resource resource) {
    this.resource = resource;
  }

  public String getCost(InvokeStmt invokeStmt) {
    //BlockMethod callee = (BlockMethod) invokeStmt.getInvokeExpr().getMethod();
    //getCost(callee);
    return cost(0);
  }

  public String getCost(BlockMethod blockMethod) {
    Cost costAnnotation = (Cost) SootUtil.getAnnotation(blockMethod, Cost.class);
    if (costAnnotation != null) {
      String function = resource.getFunction(costAnnotation);
      if (function == null) {
        Resource anyResource = Resource.ANY_RESOURCE;
        function = anyResource.getFunction(costAnnotation);
        if (function == null) {
          costAnnotation =
              (Cost) SootUtil.getAnnotation(blockMethod.getDeclaringClass(), Cost.class);
          function = anyResource.getFunction(costAnnotation);
        }
      }
      //System.out.println("Function ~~~~~~~~~~~~~~~ " + function);
      if (function != null) {
        return function;
      }
    }
    return getDefaultBlockMethodCost(blockMethod);
  }


  String getDefaultBlockMethodCost(BlockMethod blockMethod) {
      return INFINITY;
  }

  public String cost(int cost) {
    return Integer.toString(cost);
  }

  protected static String getBlockMethodClassName(BlockMethod blockMethod) {
    return blockMethod.getDeclaringClass().getName();
  }
}
