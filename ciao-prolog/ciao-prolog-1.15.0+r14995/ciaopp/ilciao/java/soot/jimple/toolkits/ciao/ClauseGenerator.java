package soot.jimple.toolkits.ciao;

import soot.BlockMethod;
import soot.Hierarchy;
import soot.IntType;
import soot.Local;
import soot.Scene;
import soot.SootClass;
import soot.SootMethod;
import soot.Type;
import soot.Unit;
import soot.Value;
import soot.VoidType;
import soot.jimple.InvokeExpr;
import soot.jimple.InvokeStmt;
import soot.jimple.Stmt;
import soot.options.CiaoOptions;
import soot.resources.annotations.Measure;
import soot.resources.annotations.Measure.MeasureType;
import soot.tagkit.Host;
import soot.util.SootUtil;
import util.Pair;

import java.io.BufferedOutputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

public class ClauseGenerator {

  private BufferedOutputStream ciaoFile;
  private static Set<String> predicatesAlreadyProcessed = new HashSet<String>();

  public void printCiao() {
    ciaoFile = SootUtil.openFileInOutputDir(getOutputFileName());
    printHeader();
    Map<String, List<BlockMethod>> methodTable = IcfgScene.getMethodMap();
    Set<String> keys = methodTable.keySet();
    sortAndPrintBlockMethods(keys, methodTable);
    SootUtil.closeStream(ciaoFile);
  }

  private void sortAndPrintBlockMethods(Collection<String> keys,
      Map<String, List<BlockMethod>> methodTable) {
    Collection<String> libraryKeys = new TreeSet<String>();
    Collection<String> nonLibraryKeys = new ArrayList<String>();
    for (String key : keys) {
      BlockMethod entryBlockMethod = IcfgScene.getEntryBlockMethod(key);
      if (entryBlockMethod.isLibrary()) {
        libraryKeys.add(key);
      } else {
        nonLibraryKeys.add(key);
      }
    }
    printBlockMethods(nonLibraryKeys, methodTable);
    printBlockMethods(libraryKeys, methodTable);
  }

  private void printBlockMethods(Collection<String> libraryKeys,
      Map<String, List<BlockMethod>> methodTable) {
    for (String key : libraryKeys) {
      List<BlockMethod> blockMethods = methodTable.get(key);
      for (BlockMethod blockMethod : blockMethods) {
        printCiaoRepresentationOf(blockMethod);
      }
    }
  }

  private void printHeader() {
    String moduleHeader = ":-module(_,";
    moduleHeader += getExportedPredicates();
    moduleHeader += ",[assertions , nativeprops , regtypes]).\n\n";
    moduleHeader += ":-set_prolog_flag(multi_arity_warnings,off).\n";
    moduleHeader += ":-set_prolog_flag(discontiguous_warnings,off).\n";
    moduleHeader += ":-set_prolog_flag(single_var_warnings,off).\n\n";
    moduleHeader += addIncludeFiles();
    moduleHeader += getImplicitelyDefined();
    SootUtil.writeToOutput(ciaoFile, moduleHeader);
  }

  private String getExportedPredicates() {
    List<String> lResult = new ArrayList<String>();
    Map<String, List<BlockMethod>> methodTable = IcfgScene.getMethodMap();
    Set<String> entryMethodNames = new TreeSet(methodTable.keySet());
    for (String entryMethodName : entryMethodNames) {
      List<BlockMethod> methods = methodTable.get(entryMethodName);
      BlockMethod entryMethod = methods.get(0);
      if (!(entryMethod.getMethod().isPrivate() || entryMethod.isLibrary())) {
        lResult.add(Namer.getFullBlockMethodNameAndArity(entryMethod));
      }
    }
    return lResult.toString();
  }

  private String addIncludeFiles() {
    String result = "";
    List<String> includeFiles = CiaoOptions.v().getIncludeFiles();
    for (String includeFile : includeFiles) {
      result += ":-include(\'" + includeFile + "\').\n";
    }
    if (CiaoOptions.v().isResourcesMode()) {
      result += ":-include(resources(res_assrt_defs)).\n";
    }
    if (result.length() > 0) {
      result += "\n";
    }
    return result;
  }

  private String getImplicitelyDefined() {
    Collection<String> libraryMethodsSignatures = new TreeSet<String>();
    List<BlockMethod> libraryBlockMethods = IcfgScene.getLibraryBlockMethods();
    for (BlockMethod libraryBlockmethod : libraryBlockMethods) {
      String blockMethodNameAndArity = Namer.getFullBlockMethodNameAndArity(libraryBlockmethod);
      libraryMethodsSignatures.add(blockMethodNameAndArity);
    }
    return ":-impl_defined(" + libraryMethodsSignatures + ").\n\n";
  }

  private void printCiaoRepresentationOf(BlockMethod blockMethod) {
    String blockMethodName = blockMethod.getBlockMethodName();
    if (!predicatesAlreadyProcessed.contains(blockMethodName)) {
      printAssertions(blockMethod);
      printOneBlockMethod(blockMethod);
      predicatesAlreadyProcessed.add(blockMethodName);
      for (BlockMethod sibling : blockMethod.getSiblings()) {
        printOneBlockMethod(sibling);
      }
    }
  }

  private void printAssertions(BlockMethod blockMethod) {
    // Remove the !isBuiltin condition to regenerate builtin Measure and Mode assertions
//    if (CiaoOptions.isResourcesMode() && !SootUtil.isBuiltin(blockMethod)) {
    if (CiaoOptions.v().isResourcesMode()) {
      printModeAssertion(blockMethod);
      printMeasureAssertion(blockMethod);
    } else {
      if (blockMethod.isEntry() && !blockMethod.isLibrary()) {
        printTypeAssertions(blockMethod);
      }
    }
  }

  /**
   * Print the Prolog representation of a BlockMethod (clause).
   *
   * @param blockMethod BlockMethod to print.
   */
  private void printOneBlockMethod(BlockMethod blockMethod) {
    if (!blockMethod.isLibrary()) {
      //printLineInformation(blockMethod);
      printClause(blockMethod);
    }
  }

  private void printTypeAssertions(BlockMethod blockMethod) {
    List<Pair<String, Set<Type>>> formalNamesAndTypes = getSetsOfTypesOfParameters(blockMethod);
    if (!formalNamesAndTypes.isEmpty()) {
      String result = ":-entry ";
      result += printPredicateHead(blockMethod) + ": ";
      List<String> namesAndTypes = new ArrayList<String>();
      for (Pair<String, Set<Type>> nameAndType : formalNamesAndTypes) {
        String formalPrologName = Namer.toProlog(nameAndType.getFirst());
        String formalPrologTypes = Namer.printQuotedTypes(nameAndType.getSecond());
        namesAndTypes.add(formalPrologName + "/" + formalPrologTypes);
      }
      result += "tau(" + namesAndTypes + ").\n";
      SootUtil.writeToOutput(ciaoFile, result);
    }
  }

  private void printModeAssertion(BlockMethod blockMethod) {
    StringBuffer modeAssertion = new StringBuffer(":-java_mode(");
    modeAssertion.append(blockMethod.getBlockMethodName());
    int numFormalParameters = blockMethod.getFormalParameterCount();
    modeAssertion.append("/" + numFormalParameters + ",");
    List<String> modesList = new ArrayList<String>();
    modesList.add("-");
    for (int i = 1; i < numFormalParameters; i++) {
      modesList.add("+");
    }
    modeAssertion.append(modesList + ").\n");
    SootUtil.writeToOutput(ciaoFile, modeAssertion.toString());
  }

  private void printMeasureAssertion(BlockMethod blockMethod) {
    StringBuffer measureAssertion = new StringBuffer(":-java_measure(");
    measureAssertion.append(Namer.getFullBlockMethodNameAndArity(blockMethod));
    measureAssertion.append(",");
    List<String> measuresList;
    if (blockMethod.isEntry()) {
      measuresList = getMeasureAssertionsForEntryBlockMethod(blockMethod);
    } else {
      measuresList = getMeasureAssertionsForNonEntryBlockMethod(blockMethod);
    }
    measureAssertion.append(measuresList);
    measureAssertion.append(").\n");
    SootUtil.writeToOutput(ciaoFile, measureAssertion.toString());
  }

  private List<String> getMeasureAssertionsForEntryBlockMethod(BlockMethod blockMethod) {
    List<Type> formalParameterTypes = blockMethod.getFormalParameterTypes();
    List<String> measuresList = new ArrayList<String>();
    Measure methodMeasureAnnotation = (Measure) SootUtil.getAnnotation(blockMethod, Measure.class);
    Type retType = formalParameterTypes.get(0);
    String retMeasure = getMeasure(methodMeasureAnnotation, retType, FormalParameter.RET);
    measuresList.add(retMeasure);
    int extraParameters = 1;
    if (!blockMethod.isStatic()) {
      Type thisType = formalParameterTypes.get(1);
      String thisMeasure = getMeasure(methodMeasureAnnotation, thisType, FormalParameter.THIS);
      measuresList.add(thisMeasure);
      extraParameters++;
    }
    int numFormalParameters = formalParameterTypes.size();
    for (int i = 0; i < numFormalParameters - extraParameters; i++) {
      Measure paramMeasureAnnotation = (Measure) SootUtil
          .getAnnotation(blockMethod, i, Measure.class);
      Type formalParameterType = formalParameterTypes.get(i + extraParameters);
      String paramMeasure =
          getMeasure(paramMeasureAnnotation, formalParameterType, FormalParameter.STANDARD);
      measuresList.add(paramMeasure);
    }
    return measuresList;
  }

  private List<String> getMeasureAssertionsForNonEntryBlockMethod(BlockMethod blockMethod) {
    List<Type> formalParameterTypes = blockMethod.getFormalParameterTypes();
    List<String> measuresList = new ArrayList<String>();
    for (Type formalParamType : formalParameterTypes) {
      String paramMeasure = getMeasure(MeasureType.DEFAULT, formalParamType);
      measuresList.add(paramMeasure);
    }
    return measuresList;
  }

  private enum FormalParameter {
    RET, THIS, STANDARD
  }

  private static String getMeasure(Measure measure, Type formalParameterType, FormalParameter fp) {
    MeasureType measureType = MeasureType.DEFAULT;
    if (measure != null) {
      switch (fp) {
        case RET: {
          measureType = measure.retMeasure();
          break;
        }
        case THIS: {
          measureType = measure.thisMeasure();
          break;
        }
        default:
        case STANDARD: {
          measureType = measure.value();
          break;
        }
      }
    }
    return getMeasure(measureType, formalParameterType);
  }

  private static String getMeasure(MeasureType measureType, Type formalParameterType) {
    MeasureType selectedMeasureType = measureType;
    if (measureType == MeasureType.DEFAULT) {
      if (formalParameterType instanceof IntType) {
        selectedMeasureType = MeasureType.INTEGER;
      } else {
        selectedMeasureType = MeasureType.SIZE;
      }
    }
    return selectedMeasureType.getMeasure();
  }

  private void printLineInformation(Host host) {
    SootUtil.writeToOutput(ciaoFile, getLineInformation(host));
  }

  private String getLineInformation(Host host) {
    int lineNumber = SootUtil.getSourceLineNumber(host);
    String result;
    if (lineNumber != -1) {
      result = "% starts at line " + lineNumber + ".\n";
    } else {
      result = "% no starting line number has been found.\n";
    }
    return result;
  }

  private List<Pair<String, Set<Type>>> getSetsOfTypesOfParameters(BlockMethod blockMethod) {
    List<Pair<String, Set<Type>>> result = new ArrayList();
    Iterator<Local> iFormalParams = blockMethod.getFormalParameters().iterator();
    if (blockMethod.getReturnType().equals(VoidType.v())) {
      iFormalParams.next();
    }
    while (iFormalParams.hasNext()) {
      Local formalParam = iFormalParams.next();
      String formalParamName = formalParam.getName();
      Set<Type> typesOfParameter = new HashSet<Type>();
      typesOfParameter.add(formalParam.getType());
      result.add(new Pair(formalParamName, typesOfParameter));
    }
    return result;
  }

  private Set<Type> getTypesOfClassesInheriting(BlockMethod blockMethod) {
    Set<Type> result = new HashSet<Type>();
    //the hierarchy has information about the original method only
    SootClass sootClass = blockMethod.getDeclaringClass();
    result.add(sootClass.getType());
    if (blockMethod.isEntry()) {
      SootMethod originalMethod = blockMethod.getMethod();
      result.addAll(getPossibleTypesOfThis(originalMethod));
    }
    return result;
  }

  private Set<Type> getPossibleTypesOfThis(SootMethod method) {
    SootClass sootClass = method.getDeclaringClass();
    Set<Type> result = new HashSet<Type>();
    Hierarchy hierarchy = Scene.v().getActiveHierarchy();
    List<SootClass> subclasses = hierarchy.getSubclassesOf(sootClass);
    for (SootClass subclass : subclasses) {
      if (isSubclassSeeingCurrentImplementation(subclass, method)) {
        result.add(subclass.getType());
      }
    }
    return result;
  }

  /**
   * Tests whether a given class inherits (or defines) a method implementation
   * If the class is a direct subclass of the one where the method is defined,
   * and it overrides the method, the test returns false, even when the subclass
   * could potentially invoke it through the 'super' keyword.
   *
   * @param subclass A subclass of the class implementing the method.
   * @param method   A SootMethod.
   * @return true If the class inherits the given implementation
   */
  private boolean isSubclassSeeingCurrentImplementation(SootClass subclass, SootMethod method) {
    Hierarchy hierarchy = Scene.v().getActiveHierarchy();
    SootMethod methodToBeCalled = hierarchy.resolveConcreteDispatch(subclass, method);
    return methodToBeCalled.equals(method);
  }

  private void printClause(BlockMethod blockMethod) {
    String head = printPredicateHead(blockMethod);
    StringBuilder clause = new StringBuilder(head);
    Collection<Unit> units = blockMethod.getActiveBody().getUnits();
    if (units.isEmpty()) {
      clause.append(".\n");
    } else {
      clause.append(":-");
      List<String> goals = new ArrayList<String>();
      for (Unit unit : units) {
        String goal = "\n\t" + printStmt(unit);
        goals.add(goal);
      }
      clause.append(SootUtil.printListWithoutBraces(goals));
      clause.append(".\n\n");
    }
    SootUtil.writeToOutput(ciaoFile, clause.toString());
  }

  private static String printStmt(Unit unit) {
    if (unit instanceof InvokeStmt) {
      return printInvocation(((Stmt) unit).getInvokeExpr());
    } else {
      throw new RuntimeException(
          "Statement: " + unit + " in the iCFG seems not to be an invocation.");
    }
  }

  /**
   * Print an invocation to a block method in the iCFG.
   *
   * @param invokeExpr InvokeExpr including This and Res
   * @return String in the form methodName(param1,...,paramN)
   */
  private static String printInvocation(InvokeExpr invokeExpr) {
    BlockMethod method = (BlockMethod) invokeExpr.getMethod();
    String methodName = method.getBlockMethodName();
    String params = printParameters(invokeExpr);
    return methodName + params;
  }

  private static String printParameters(InvokeExpr invokeExpr) {
    List<String> lResult = new ArrayList<String>();
    List<Value> args = invokeExpr.getArgs();
    for (Value arg : args) {
      lResult.add(Namer.toProlog(arg));
    }
    return SootUtil.listToStringTuple(lResult);
  }

  private static String printPredicateHead(BlockMethod blockMethod) {
    String clause = blockMethod.getBlockMethodName();
    clause += getParameterNames(blockMethod);
    return clause;
  }

  private static String getParameterNames(BlockMethod method) {
    List<String> lResult = new ArrayList<String>();
    List<Local> formalParams = method.getFormalParameters();
    for (Local formalParam : formalParams) {
      lResult.add(Namer.toProlog(formalParam.getName()));
    }
    return SootUtil.listToStringTuple(lResult);
  }

  public static String getOutputFileName() {
    return Scene.v().getMainClass().getName() + ".pl";
  }
}


