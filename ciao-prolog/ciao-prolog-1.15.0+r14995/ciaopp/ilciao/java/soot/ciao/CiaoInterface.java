package soot.ciao;

import soot.BlockMethod;
import soot.CiaoMain;
import soot.Local;
import soot.PatchingChain;
import soot.Type;
import soot.Unit;
import soot.jimple.InvokeStmt;
import soot.jimple.toolkits.ciao.IcfgScene;
import soot.jimple.toolkits.ciao.Namer;
import soot.options.CiaoOptions;
import soot.resources.ResourceFactory;
import soot.resources.ResourceFunction;
import soot.tagkit.Host;
import soot.util.SootUtil;
import util.Pair;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Set;


public class CiaoInterface {

  public static void main(String[] args) {
    CiaoInterface ciaoInterface = new CiaoInterface();
    ciaoInterface.generateCiao("/home/mario/mySVN/CiaoDE/ciaopp/ilciao", "examples.CellPhone");
    System.out.println(">>>>>>>>>>>>>>>>>>>>>>>>>>> " + ciaoInterface.getResources());
    Collection<BlockMethod> bms = ciaoInterface.getEntryBlockMethodsSortedByLineNumber();
    for (BlockMethod bm : bms) {
      System.out.println(">>>>>>>>>>>>>>>>>>>>> " + ciaoInterface.getFormalParameterNames(bm));
      System.out.println(">>>>>>>>>>>>>>>>>>>>> " + bm.getBlockMethodName());
      System.out.println(">>>>> " + SootUtil.getSourceLineNumber(bm));
      System.out.println("Cost 1>>>>>>>>>>>>>>>> " + ciaoInterface
          .getCost("ENERGY_CONSUMED", "'lang.Builtin.sta.25#0'/7/1"));
      System.out.println("Cost 2>>>>>>>>>>>>>>>> " +
          ciaoInterface.getCost("SIZE", bm.getBlockMethodName() + "/2/1"));
    }
//    System.out.println(">>>>>>>>>>>>>>>>>>>>>>>>>>> " +
//        ciaoInterface.getFormalParamsTypes("usr:examples.MyVector$add#1143406767#0"));
  }

  public String getResources() {
    try {
      Collection<String> resourceNames = ResourceFactory.getInstance().getResourceNames();
      return resourceNames.toString();
    } catch (RuntimeException ex) {
      printExceptionToCiaoConsole(ex);
      throw new RuntimeException(ex);
    }
  }

  public void generateCiao(String baseDir, String className) {
    try {
      CiaoMain.generateCiao(baseDir, className);
    } catch (RuntimeException ex) {
      printExceptionToCiaoConsole(ex);
    }
  }

  /**
   * Given a clause or term identifier and a resource,
   * return the resource cost of the corresponding block method or unit.
   *
   * @param resourceName A resource name.
   * @param termId       An identifier of a clause or term in the Prolog equivalent of the iCFG.
   * @return CostInDollars of the block method or unit for the given resource.
   */
  public String getCost(String resourceName, String termId) {
    try {
      checkTermIdSyntax(termId);
      BlockMethod blockMethod = getBlockMethod(termId);
      if (!isTermId(termId)) {
        return ResourceFactory.getInstance().getCost(resourceName, blockMethod);
      } else {
        Pair<InvokeStmt, Integer> invokeStmtAndInt = getUnit(blockMethod, termId);
        InvokeStmt invokeStmt = invokeStmtAndInt.getFirst();
        return ResourceFactory.getInstance().getCost(resourceName, invokeStmt);
      }
    } catch (RuntimeException rex) {
      printExceptionToCiaoConsole(rex);
      System.err.println("...returning INFINITY...");
      return ResourceFunction.INFINITY;
    }
  }

  /**
   * Check the validity of a Prolog identifier.
   *
   * @param clauseOrTermId Clause (name/arity/num) or Term (name/arity/num/num) identifier.
   */
  private static void checkTermIdSyntax(String clauseOrTermId) {
    String[] components = clauseOrTermId.split("/");
    int numComponents = components.length;
    if (numComponents != 3 && numComponents != 4) {
      throw new IllegalArgumentException(
          "Incorrect syntax (not x/y/z or z/y/z/t) for atom: " + clauseOrTermId);
    }
    String[] nameComponents = components[0].split("#");
    int numNameParts = nameComponents.length;
    if (numNameParts < 2) {
      throw new IllegalArgumentException("Incorrect blockMethod name (not in the form name.id" +
          "#num(#num)*) within atom " + clauseOrTermId);
    }
  }

  private boolean isTermId(String termId) {
    String[] components = termId.split("/");
    return components.length == 4;
  }

  /**
   * Given a clause or term identifier, return the corresponding block method.
   * If the identifier designates a clause, the equivalent block method is returned.
   * If the identifier is a term, the block method to which the equivalent unit belongs to is
   * returned.
   *
   * @param clauseOrTermId Prolog (syntactically correct) clause or term identifier.
   * @return BlockMethod Block method whose Prolog equivalent is the clause
   *         designated by the identifier.
   */
  public static BlockMethod getBlockMethod(String clauseOrTermId) {
    checkTermIdSyntax(clauseOrTermId);
    String entryBlockMethodName = getEntryBlockMethodName(clauseOrTermId);
    List<BlockMethod> blockMethods =
        IcfgScene.getBlockMethodListFromEntryBlockMethodName(entryBlockMethodName);
    return getBlockMethod(clauseOrTermId, blockMethods);
  }

  private static String getEntryBlockMethodName(String clauseOrTermId) {
    String[] components = clauseOrTermId.split("/");
    String blockMethodName = components[0].replaceAll("'", "");
    String[] nameComponents = blockMethodName.split("#");
    return SootUtil.addQuotes(nameComponents[0] + "#0");
  }

  private static BlockMethod getBlockMethod(String clauseOrTermId, List<BlockMethod> blockMethods) {
    String[] components = clauseOrTermId.split("/");
    String blockMethodName = components[0].replaceAll("'", "");
    int numClause = Integer.parseInt(components[2]);
    String[] nameComponents = blockMethodName.split("#");
    int indexInMethod = Integer.parseInt(nameComponents[numClause]);
    return blockMethods.get(indexInMethod);
  }

  /**
   * Given a block method and a term identifier, return the corresponding unit in the iCFG
   * and its position on the enclosing block method.
   *
   * @param blockMethod Block method the unit is known to belong to.
   * @param termId      Syntactically correct term id.
   * @return Unit (Invocation) equivalent in the iCFG for the given term in the Prolog
   *         program and position of that statement within the block method.
   */
  public static Pair<InvokeStmt, Integer> getUnit(BlockMethod blockMethod, String termId) {
    PatchingChain units = blockMethod.getActiveBody().getUnits();
    String[] components = termId.split("/");
    int numUnit = Integer.parseInt(components[3]);
    if (numUnit > units.size()) {
      throw new IllegalArgumentException("Tried to retrieve unit #" + numUnit +
          ", but block method " + blockMethod.getBlockMethodName() + " contains only " +
          units.size() + " unit(s).");
    } else {
      List<Unit> unitsAsList = new ArrayList<Unit>(units);
      InvokeStmt invokeStmt = (InvokeStmt) unitsAsList.get(numUnit - 1);
      return new Pair(invokeStmt, numUnit);
    }
  }

  /**
   * Given a block method name, return a list with the Prolog
   * string representation of the types of its formal parameters.
   *
   * @param blockMethodName In the format 'usr:name#num[#num]?#id'.
   * @return List with the Prolog representations of the formal parameters types.
   */
  public List<String> getFormalParamsTypes(String blockMethodName) {
    try {
      return getFormalParamsTypes_(blockMethodName);
    } catch (RuntimeException ex) {
      printExceptionToCiaoConsole(ex);
      throw ex;
    }
  }

  private List<String> getFormalParamsTypes_(String blockMethodName) {
    //    blockMethodName = blockMethodName.replaceFirst("usr:", "");
    blockMethodName = removeModuleName(blockMethodName);  
    blockMethodName += "/1/1";
    BlockMethod blockMethod = getBlockMethod(blockMethodName);
    List<String> result = new ArrayList<String>();
    for (Type formalParamType : blockMethod.getFormalParameterTypes()) {
      String typePrologName = Namer.printType(formalParamType);
      result.add(typePrologName);
    }
    return result;
  }

  private String removeModuleName(String str){

      String[] words = str.split(":");
      if (words.length == 0) {
	  // it should not happen ...
	  return str;
      } else if (words.length == 1){
	  return words[0];
      } else {
	  return words[1];
      }
  }
    
  public List<String> getFormalParameterNames(BlockMethod blockMethod) {
    if (!blockMethod.isLibrary()) {
      List<String> result = new ArrayList<String>();
      if (CiaoOptions.v().isPreservingOriginalNames()) {
        List<Local> formalParameters = blockMethod.getFormalParameters();
        for (Local local : formalParameters) {
          result.add(local.getName());
        }
      } else {
        int start = 1;
        result.add(Namer.RET);
        if (!blockMethod.isStatic()) {
          result.add(Namer.THIS);
          start++;
        }
        for (int i = start; i < blockMethod.getFormalParameterCount(); i++) {
          result.add("arg(" + (i - start + 1) + ")");
        }
      }
      return result;
    }
    throw new RuntimeException("Block method " + blockMethod.getBlockMethodName() +
        "is of type library, and therefore its formal parameter names could not be retrieved");
  }

  public List<BlockMethod> getEntryBlockMethodsSortedByLineNumber() {
    List<BlockMethod> result = new ArrayList<BlockMethod>();
    Set<String> keys = IcfgScene.getMethodMap().keySet();
    for (String key : keys) {
      BlockMethod entryBlockMethod = IcfgScene.getEntryBlockMethod(key);
      if (!(SootUtil.isConstructor(entryBlockMethod) || entryBlockMethod.isLibrary() ||
          entryBlockMethod.isProxy())) {
        result.add(entryBlockMethod);
      }
    }
    Collections.sort(result, new HostComparator());
    return result;
  }

  private class HostComparator implements Comparator {
    public int compare(Object o, Object o1) {
      Host host1 = (Host) o;
      Host host2 = (Host) o1;
      Integer lineStart1 = SootUtil.getSourceLineNumber(host1);
      Integer lineStart2 = SootUtil.getSourceLineNumber(host2);
      if (lineStart1 == -1) {
        return 1;
      } else if (lineStart2 == -1) {
        return -1;
      } else {
        return lineStart1 - lineStart2;
      }
    }
  }

  private static void printExceptionToCiaoConsole(Exception ex) {
    //TODO: FIXME
    //PrintStream errorFile = new PrintStream(System.err);
    //ex.printStackTrace(errorFile);
    //errorFile.close();
  }

  public static void printMessageToCiaoConsole(String msg) {
    PrintStream errorFile = new PrintStream(System.out);
    errorFile.print(msg);
    errorFile.close();
  }

}
