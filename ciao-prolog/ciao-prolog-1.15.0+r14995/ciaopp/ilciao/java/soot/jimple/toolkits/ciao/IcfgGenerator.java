package soot.jimple.toolkits.ciao;

import soot.BlockMethod;
import soot.Body;
import soot.BodyTransformer;
import soot.Hierarchy;
import soot.Immediate;
import soot.Local;
import soot.RefLikeType;
import soot.Scene;
import soot.SootClass;
import soot.SootField;
import soot.SootMethod;
import soot.SootMethodRef;
import soot.Type;
import soot.Unit;
import soot.Value;
import soot.VoidType;
import soot.jimple.*;
import soot.options.CiaoOptions;
import soot.toolkits.graph.Block;
import soot.toolkits.graph.ExceptionalBlockGraph;
import soot.util.Chain;
import soot.util.SootUtil;
import util.Pair;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

public class IcfgGenerator extends BodyTransformer {

  private static IcfgGenerator instance;
  private static boolean firstTime = true;

  private IcfgGenerator() {
  }

  public static IcfgGenerator v() {
    if (instance == null) {
      instance = new IcfgGenerator();
    }
    return instance;
  }

  protected void internalTransform(Body body, String phaseName, Map options) {
    if (firstTime) {
      IcfgScene.v().createScene();
      firstTime = false;
    }
    SootMethod method = body.getMethod();
    if (!SootUtil.isBuiltin(method) && method.isConcrete()) {
      fillMethods(body);
    }
  }

  private void fillMethods(Body body) {
    ExceptionalBlockGraph blockGraph = new ExceptionalBlockGraph(body);
    List<Block> blocks = blockGraph.getBlocks();
    Block entryBlock = blocks.get(0);
    BlockMethod entryBlockMethod = IcfgScene.getBlockMethodFromBlock(entryBlock);
    List<BlockMethod> blockMethods =
        IcfgScene.getBlockMethodListFromEntryBlockMethodName(entryBlockMethod.getBlockMethodName());
    for (BlockMethod blockMethod : blockMethods) {
      getBlockMethodFromBlock(blockMethod);
    }
    IcfgSImplifier.simplifyIcfg(blockMethods);
    setFormalParameters(blockMethods);
    addCallsToNextBlock(blockMethods);
  }

  private void getBlockMethodFromBlock(BlockMethod blockMethod) {
    SootMethod originalMethod = blockMethod.getMethod();
    StmtBody body = Jimple.v().newBody(blockMethod);
    blockMethod.setActiveBody(body);
    Chain locals = body.getLocals();
    locals.addAll(getBodyLocals(originalMethod));
    getBodyUnits(blockMethod);
  }

  private Collection<Local> getBodyLocals(SootMethod method) {
    List<Local> locals = new ArrayList<Local>();
    String retName;
    Type returnType = method.getReturnType();
    if (returnType.equals(VoidType.v())) {
      retName = "void";
    } else {
      retName = Namer.RET;
    }
    Local res = getNewVariable(retName, returnType);
    locals.add(res);
    locals.addAll(method.getActiveBody().getLocals());
    return locals;
  }

  private void getBodyUnits(BlockMethod blockMethod) {
    Chain units = blockMethod.getActiveBody().getUnits();
    units.addAll(getAssigmentsToShadowVars(blockMethod));
    units.addAll(getBlockMethodUnits(blockMethod));
    List<Unit> guardUnits = getGuard(blockMethod);
    if (!guardUnits.isEmpty()) { //Chain does not provide addAll
      units.addFirst(guardUnits.get(0));
    }
  }

  private List<Unit> getAssigmentsToShadowVars(BlockMethod blockMethod) {
    List<Unit> result = new ArrayList<Unit>();
    if (blockMethod.isEntry()) {
      SootMethod originalMethod = blockMethod.getMethod();
      int numParams = originalMethod.getParameterCount();
      Collection<Local> locals = blockMethod.getActiveBody().getLocals();
      Iterator iLocals = locals.iterator();
      int startingIndex = SootUtil.getParameterCountOfLibraryMethod(originalMethod) - 1;
      for (int i = startingIndex; i < numParams; i++) {
        Local param = (Local) iLocals.next();
        Type type = param.getType();
        if (type instanceof RefLikeType) {
          String shadowName = param.getName() + "_";
          Local shadowVar = getNewVariable(shadowName, type);
          Unit assignStmt = Jimple.v().newAssignStmt(shadowVar, param);
          result.add(assignStmt);
        }
      }
    }
    return result;
  }

  private List<Unit> getBlockMethodUnits(BlockMethod blockMethod) {
    List<Unit> result = new ArrayList<Unit>();
    Block block = blockMethod.getBlock();
    if (block != null) {
      Iterator<Unit> iUnit = block.iterator();
      while (iUnit.hasNext()) {
        Unit unit = iUnit.next();
        Unit clone = (Unit) unit.clone();
        clone.addAllTagsOf(unit);
        result.addAll(processUnit(clone, unit, blockMethod));
      }
    }
    return result;
  }

  private List<Unit> processUnit(Unit unit, Unit originalUnit, BlockMethod blockMethod) {
    List<Unit> result = new ArrayList<Unit>();
    Unit newUnit = null;
    if (unit instanceof IdentityStmt) {

    } else if (unit instanceof GotoStmt) {

    } else if (unit instanceof IfStmt) { //processed later by the successors

    } else if (unit instanceof ReturnVoidStmt) {//return

    } else if (unit instanceof ReturnStmt) {//return x
      newUnit = transformReturnIntoInvoke((ReturnStmt) unit, blockMethod);

    } else if (unit instanceof InvokeStmt) {//call to void method
      InvokeStmt invokeStmt = (InvokeStmt) unit;
      newUnit = transformVoidInvocation(invokeStmt);

    } else if (unit instanceof AssignStmt) {//x=expr
      newUnit = transformAssignmentIntoInvoke((AssignStmt) unit);

    } else {
      //throw new RuntimeException("Unsupported unit: " + unit);
      newUnit = getInvokeFromBuiltin(Builtin.UNK, getVoidVariable(), unit.toString());
    }
    if (newUnit != null) {
      // Writing newUnit.addAllTagsOf(unit) results in ComodificationException.
      newUnit.addAllTagsOf(originalUnit);
      result.add(newUnit);
    }
    return result;
  }

  private static Unit transformAssignmentIntoInvoke(AssignStmt assignStmt) {
    Value lhs = assignStmt.getLeftOp();
    Value rhs = assignStmt.getRightOp();
    if (lhs instanceof Local) {

      if (rhs instanceof ArrayRef) { // lhs = base[index]
        ArrayRef arrayRef = (ArrayRef) rhs;
        Value base = arrayRef.getBase();
        Value index = arrayRef.getIndex();
        return getInvokeFromBuiltin(Builtin.GTA, lhs, lhs.getType(), base, index,
            arrayRef.getType());

      } else if (rhs instanceof BinopExpr) { //assign to binop
        BinopExpr binopExpr = (BinopExpr) rhs;
        Builtin builtin = getArithmeticBuiltin(binopExpr);
        Value op1 = binopExpr.getOp1();
        Value op2 = binopExpr.getOp2();
        return getInvokeFromBuiltin(builtin, lhs, lhs.getType(), op1, op1.getType(), op2,
            op2.getType());

      } else if (rhs instanceof CastExpr) {
        CastExpr castExpr = (CastExpr) rhs;
        Value rop = castExpr.getOp();
        return getInvokeFromBuiltin(Builtin.CAST, lhs, castExpr.getCastType(), rop, rop.getType());

      } else if (rhs instanceof Immediate) { //assign to local or constant
        Builtin builtin = getBuiltinSpecializedVersion(Builtin.ASG, lhs.getType());
        return getInvokeFromBuiltin(builtin, lhs, lhs.getType(), rhs, rhs.getType());

      } else if (rhs instanceof InstanceFieldRef) { //getfield
        InstanceFieldRef instanceFieldRef = (InstanceFieldRef) rhs;
        Value base = instanceFieldRef.getBase();
        SootField field = instanceFieldRef.getField();
        SetfieldTag valueTag = (SetfieldTag) assignStmt.getTag("setfieldTag");
        if (CiaoOptions.v().isResourcesMode() && valueTag != null) {
          Value value = valueTag.getAssignedValue();
          return getInvokeFromBuiltin(Builtin.STF, lhs, base, base.getType(), field.getName(),
              field.getType(), value, value.getType());
        } else {
          return getInvokeFromBuiltin(Builtin.GTF, lhs, lhs.getType(), base, base.getType(),
              field.getName(), field.getType());
        }

      } else if (rhs instanceof InstanceOfExpr) { //instanceof
        InstanceOfExpr instanceOfExpr = (InstanceOfExpr) rhs;
        return getInvokeFromBuiltin(Builtin.IOF, lhs, lhs.getType(), instanceOfExpr.getOp(),
            instanceOfExpr.getCheckType());

      } else if (rhs instanceof InvokeExpr) { //x = call(...)
        return transformNonVoidInvocation(assignStmt);

      } else if (rhs instanceof NewArrayExpr) { //new type[size]
        NewArrayExpr newArrayExpr = (NewArrayExpr) rhs;
        Value size = newArrayExpr.getSize();
        return getInvokeFromBuiltin(Builtin.NEWA, lhs, lhs.getType(), size);

      } else if (rhs instanceof NewExpr) { //new
        return getInvokeFromBuiltin(Builtin.NEWB, lhs, rhs.getType());

      } else {
        return getInvokeFromBuiltin(Builtin.UNK, getVoidVariable(), assignStmt.toString());
      }

    } else if (lhs instanceof InstanceFieldRef) { //setfield
      if (CiaoOptions.v().isResourcesMode()) {
        return null; //the setfields are there just to keep the variable references
      } else {
        InstanceFieldRef fieldRef = (InstanceFieldRef) lhs;
        Value base = fieldRef.getBase();
        SootField field = fieldRef.getField();
        return getInvokeFromBuiltin(Builtin.STF, getVoidVariable(), base, base.getType(),
            field.getName(), field.getType(), rhs, rhs.getType());
      }

    } else if (lhs instanceof ArrayRef) { //base[index]=rhs
      ArrayRef arrayRef = (ArrayRef) lhs;
      Value base = arrayRef.getBase();
      Value index = arrayRef.getIndex();
      return getInvokeFromBuiltin(Builtin.STA, base, index, arrayRef.getType(), rhs, rhs.getType());

    } else {
      return getInvokeFromBuiltin(Builtin.UNK, getVoidVariable(), assignStmt.toString());
    }
  }

  private static Builtin getArithmeticBuiltin(BinopExpr binopExpr) {
    Builtin result = Builtin.UNK;
    if (binopExpr instanceof AddExpr) {
      result = Builtin.ADD;
    } else if (binopExpr instanceof DivExpr) {
      result = Builtin.DIV;
    } else if (binopExpr instanceof MulExpr) {
      result = Builtin.MUL;
    } else if (binopExpr instanceof RemExpr) {
      result = Builtin.REM;
    } else if (binopExpr instanceof ShlExpr) {
      result = Builtin.SHL;
    } else if (binopExpr instanceof ShrExpr) {
      result = Builtin.SHR;
    } else if (binopExpr instanceof SubExpr) {
      result = Builtin.SUB;
    }
    return result;
  }

  private Builtin getBuiltinFromConditionExpr(ConditionExpr condition) {
    Builtin builtin = Builtin.UNK;
    if (condition instanceof EqExpr) {
      builtin = Builtin.EQ;
    } else if (condition instanceof NeExpr) {
      builtin = Builtin.NE;
    } else if (condition instanceof GeExpr) {
      builtin = Builtin.GE;
    } else if (condition instanceof GtExpr) {
      builtin = Builtin.GT;
    } else if (condition instanceof LeExpr) {
      builtin = Builtin.LE;
    } else if (condition instanceof LtExpr) {
      builtin = Builtin.LT;
    }
    return builtin;
  }

  private static Builtin getBuiltinSpecializedVersion(Builtin builtin, Type type) {
    if (CiaoOptions.v().isResourcesMode()) {
      if (type instanceof RefLikeType) {
        builtin = builtin.getSpecializedSizeVersion();
      } else {
        builtin = builtin.getSpecializedIntVersion();
      }
    }
    return builtin;
  }

  private static Unit transformReturnIntoInvoke(ReturnStmt returnStmt, SootMethod method) {
    Type resType = method.getReturnType();
    Local res = getNewVariable("ret", resType);
    Immediate rop = (Immediate) returnStmt.getOp();
    Builtin builtin = getBuiltinSpecializedVersion(Builtin.ASG, resType);
    return getInvokeFromBuiltin(builtin, res, resType, rop, rop.getType());
  }

  private static InvokeStmt getInvokeFromBuiltin(Builtin builtin, Object... pars) {
    BlockMethod method = IcfgScene.getBuiltinMethod(builtin);
    SootMethodRef methodRef = method.makeRef();
    List<Immediate> args = transformParameters(pars);
    InvokeExpr invokeExpr = Jimple.v().newStaticInvokeExpr(methodRef, args);
    return Jimple.v().newInvokeStmt(invokeExpr);
  }

  private static List<Immediate> transformParameters(Object... params) {
    List<Immediate> result = new ArrayList<Immediate>();
    for (Object o : params) {
      Immediate parameter;
      if (o instanceof Immediate) {
        parameter = (Immediate) o;
      } else if (o instanceof Type) {
        String typeAsString = Namer.printType((Type) o);
        parameter = StringConstant.v(typeAsString);
      } else if (o instanceof String) {
        String s = (String) o;
        parameter = StringConstant.v(SootUtil.addQuotes(s));
      } else {
        throw new RuntimeException("Unexpect object as invocation parameter: " + o);
      }
      result.add(parameter);
    }
    return result;
  }

  private static Unit transformVoidInvocation(InvokeStmt invokeStmt) {
    InvokeExpr invokeExpr = invokeStmt.getInvokeExpr();
    InvokeExpr newInvokeExpr = transformInvocation(invokeExpr, getVoidVariable());
    invokeStmt.setInvokeExpr(newInvokeExpr);
    return invokeStmt;
  }

  private static Unit transformNonVoidInvocation(AssignStmt assignStmt) {
    Local assignedVar = (Local) assignStmt.getLeftOp();
    InvokeExpr newInvokeExpr = transformInvocation(assignStmt.getInvokeExpr(), assignedVar);
    return Jimple.v().newInvokeStmt(newInvokeExpr);
  }

  private static InvokeExpr transformInvocation(InvokeExpr invokeExpr, Local returnVar) {
    List<Local> formalParams = invokeExpr.getArgs(); //deep copy
    formalParams.add(0, returnVar);
    SootMethod methodInvoked = invokeExpr.getMethod();
    SootClass baseClass = methodInvoked.getDeclaringClass();
    if (baseClass.isApplicationClass()) {
      if (invokeExpr instanceof VirtualInvokeExpr || invokeExpr instanceof InterfaceInvokeExpr) {
        InstanceInvokeExpr instanceInvokeExpr = (InstanceInvokeExpr) invokeExpr;
        Local base = (Local) instanceInvokeExpr.getBase();
        formalParams.add(1, base);
        methodInvoked = addProxyBlockMethods(methodInvoked, base, formalParams);
      }
    } else {
      IcfgScene.newLibraryBlockMethod(methodInvoked);
    }
    BlockMethod blockMethodInvoked = IcfgScene.getEntryBlockMethodFromMethod(methodInvoked);
    return instantiateInvocation(invokeExpr, returnVar, blockMethodInvoked);
  }

  private static InvokeExpr instantiateInvocation(InvokeExpr invokeExpr, Local returnVar,
      BlockMethod blockMethodInvoked) {
    InvokeExpr result = null;
    List<Local> actualParams = invokeExpr.getArgs();
    actualParams.add(0, returnVar);
    SootMethodRef blockMethodRef = blockMethodInvoked.makeRef();
    if (blockMethodInvoked.isStatic()) {
      result = Jimple.v().newStaticInvokeExpr(blockMethodRef, actualParams);
    } else {
      InstanceInvokeExpr instanceInvokeExpr = (InstanceInvokeExpr) invokeExpr;
      Local base = (Local) instanceInvokeExpr.getBase();
      actualParams.add(1, base);
      if (invokeExpr instanceof VirtualInvokeExpr || invokeExpr instanceof InterfaceInvokeExpr) {
        result = Jimple.v().newVirtualInvokeExpr(base, blockMethodRef, actualParams);
      } else if (invokeExpr instanceof SpecialInvokeExpr) {
        result = Jimple.v().newSpecialInvokeExpr(base, blockMethodRef, actualParams);
      }
    }
    return result;
  }

  private static SootMethod addProxyBlockMethods(SootMethod methodInvoked, Local base,
      List<Local> args) {
    SootClass baseClass = methodInvoked.getDeclaringClass();
    Hierarchy hierarchy = Scene.v().getActiveHierarchy();
    List<SootMethod> implementations = hierarchy.resolveAbstractDispatch(baseClass, methodInvoked);
    SootMethod result;
    int numImplementations = implementations.size();
    switch (numImplementations) {
      case 0: {
        throw new RuntimeException("No implementations found for method " + methodInvoked);
      }
      case 1: { //avoid the extra block method
        result = implementations.get(0);
        break;
      }
      default: {
        String proxyMethodName = Namer.getFullBlockMethodNameForMethod(methodInvoked);
        if (!IcfgScene.containsEntryBlockMethod(proxyMethodName)) {//already processed
          int index = 0;
          for (SootMethod implementation : implementations) {
            newProxyBlockMethod(methodInvoked, implementation, base, args, index);
            index++;
          }
          setProxyRelationships(proxyMethodName);
        }
        result = methodInvoked;
      }
    }
    return result;
  }

  private static void setProxyRelationships(String proxyMethodName) {
    List<BlockMethod> proxies =
        IcfgScene.getBlockMethodListFromEntryBlockMethodName(proxyMethodName);
    for (BlockMethod proxy : proxies) {
      for (BlockMethod sibling : proxies) {
        if (!sibling.equals(proxy)) {
          proxy.getSiblings().add(sibling);
        }
      }
    }
  }

  private static BlockMethod newProxyBlockMethod(SootMethod methodInvoked,
      SootMethod implementation, Local base, List<Local> args, int indexInMethod) {
    List<Type> parameterTypes = SootUtil.getTypes(args);
    System.out.println(">>>>>>      >>>??? " + SootUtil.getSourceLineNumber(methodInvoked));
    BlockMethod proxy = IcfgScene.newBlockMethod(methodInvoked, parameterTypes,
        BlockMethod.BlockMethodType.PROXY, indexInMethod);
    StmtBody body = Jimple.v().newBody(proxy);
    proxy.setActiveBody(body);
    body.getLocals().addAll(args);
    SootMethod method = IcfgScene.getEntryBlockMethodFromMethod(implementation);
    InvokeStmt invokeStmt = getInvocationToImplementation(method, base, args);
    body.getUnits().add(invokeStmt);
    String entryBlockMethodName = Namer.getFullBlockMethodNameForMethod(methodInvoked);
    IcfgScene.addBlockMethod(entryBlockMethodName, proxy);
    return proxy;
  }

  private static InvokeStmt getInvocationToImplementation(SootMethod implementation, Local base,
      List<Local> args) {
    SootMethodRef implementationRef = implementation.makeRef();
    InvokeExpr invokeExpr = Jimple.v().newVirtualInvokeExpr(base, implementationRef, args);
    return Jimple.v().newInvokeStmt(invokeExpr);
  }


  private static void addCallsToNextBlock(List<BlockMethod> blockMethods) {
    for (BlockMethod blockMethod : blockMethods) {
      Chain units = blockMethod.getActiveBody().getUnits();
      units.addAll(getCallToNextBlock(blockMethod));
    }
  }

  private static List<Unit> getCallToNextBlock(BlockMethod blockMethod) {
    List<Unit> result = new ArrayList<Unit>();
    Set<BlockMethod> succs = blockMethod.getSuccs();
    if (!succs.isEmpty()) {
      BlockMethod succ = succs.iterator().next();
      List parameters = succ.getFormalParameters();
      InvokeExpr callExpr;
      if (succ.isStatic()) {
        callExpr = Jimple.v().newStaticInvokeExpr(succ.makeRef(), parameters);
      } else {
        Local thisLocal = blockMethod.getFormalParameters().get(1);
        callExpr = Jimple.v().newVirtualInvokeExpr(thisLocal, succ.makeRef(), parameters);
      }
      InvokeStmt callStmt = Jimple.v().newInvokeStmt(callExpr);
      result.add(callStmt);
    }
    return result;
  }

  private List<Unit> getGuard(BlockMethod blockMethod) {
    List<Unit> result = new ArrayList<Unit>();
    IfStmt guardStmt = blockMethod.getGuard();
    if (guardStmt != null) {
      ConditionExpr condition = (ConditionExpr) guardStmt.getCondition();
      Value lop = condition.getOp1();
      Value rop = condition.getOp2();
      Builtin builtin = getBuiltinFromConditionExpr(condition);
      builtin = getBuiltinSpecializedVersion(builtin, lop.getType());
      Unit invokeStmt =
          getInvokeFromBuiltin(builtin, getVoidVariable(), lop, lop.getType(), rop, rop.getType());
      result.add(invokeStmt);
    }
    return result;
  }

  private static Local getVoidVariable() {
    return Jimple.v().newLocal(VoidType.v().toString(), VoidType.v());
  }

  private static Local getNewVariable(String name, Type type) {
    return Jimple.v().newLocal(name, type);
  }

  static boolean isEntryBlock(Block block) {
    return block.getIndexInMethod() == 0;
  }

  private static void setFormalParameters(List<BlockMethod> blockMethods) {
    if (!CiaoOptions.v().isProject()) {
      setFormalParametersWithoutProjecting(blockMethods);
    } else {
      setFormalParametersProjecting(blockMethods);
    }
  }

  private static void setFormalParametersWithoutProjecting(List<BlockMethod> blockMethods) {
    for (BlockMethod blockMethod : blockMethods) {
      if (!blockMethod.isEntry()) {
        List<Local> formalParameters =
            new ArrayList<Local>(blockMethod.getActiveBody().getLocals());
        blockMethod.setFormalParameters(formalParameters);
      }
    }
  }

  /**
   * The ordered list of parameters of a blockMethod is the sum of
   * <ul>
   * <li>Variables that are read by the block method or its successors
   * without being written inside it.</li>
   * <li>Variables that are read by any sibling or its successors
   * without being written inside the sibling.</li>
   * <li>The return variable and, if applicable, the this variable.</li>
   * </ul>
   * The order imposed in the parameters is: return variable, this variable
   * (if applicable), rest of variables (in alphabetical order).
   *
   * @param blockMethods List of blockMethods in the iCFG.
   */
  private static void setFormalParametersProjecting(List<BlockMethod> blockMethods) {
    Set<Local>[] blockIdToRead = getLocalsReadByBlock(blockMethods);
    int id = 0;
    for (BlockMethod blockMethod : blockMethods) {
      if (!blockMethod.isEntry()) {
        Set<Local> paramsNeeded = new TreeSet<Local>(SootUtil.LOCAL_COMPARATOR);
        paramsNeeded.addAll(blockIdToRead[id]);
        Collection<BlockMethod> siblings = blockMethod.getSiblings();
        for (BlockMethod sibling : siblings) {
          int siblingId = sibling.getIndexInMethod();
          paramsNeeded.addAll(blockIdToRead[siblingId]);
        }
        Collection<Local> retAndThis = getRetAndThis(blockMethod);
        paramsNeeded.removeAll(retAndThis);
        List<Local> formalParams = new ArrayList<Local>(paramsNeeded);
        formalParams.addAll(0, retAndThis);
        blockMethod.setFormalParameters(formalParams);
        List<Type> paramTypes = new ArrayList<Type>(SootUtil.getTypes(formalParams));
        blockMethod.setParameterTypes(paramTypes);
      }
      id++;
    }
  }

  /**
   * Implementation of the live variable analysis algorithm to retrieve the set of
   * variables that a block method needs to have as formal parameters, which is the
   * set of variables that are live at the beginning of it.
   *
   * @param blockMethods List of block methods corresponding to a same method.
   * @return Set of local variables that each block method needs to read from its
   *         predecessors.
   */
  private static Set<Local>[] getLocalsReadByBlock(List<BlockMethod> blockMethods) {
    int numBlockMethods = blockMethods.size();
    Pair<Set<Local>, Set<Local>>[] blockIdToReadWrite = new Pair[numBlockMethods];
    for (BlockMethod blockMethod : blockMethods) {
      int id = blockMethod.getIndexInMethod();
      Pair<Set<Local>, Set<Local>> readWrite = getReadWrite(blockMethod);
      blockIdToReadWrite[id] = readWrite;
    }
    Set<Local>[] blockIdToIn = new Set[numBlockMethods];
    for (int i = 0; i < numBlockMethods; i++) {
      blockIdToIn[i] = new TreeSet(SootUtil.LOCAL_COMPARATOR);
      blockIdToIn[i].addAll(blockIdToReadWrite[i].getFirst());
    }
    boolean changed;
    do {
      changed = false;
      for (BlockMethod blockMethod : blockMethods) {
        int id = blockMethod.getIndexInMethod();
        Set<Local> out = new TreeSet(SootUtil.LOCAL_COMPARATOR);
        Collection<BlockMethod> succs = blockMethod.getSuccs();
        for (BlockMethod succ : succs) {
          int succId = succ.getIndexInMethod();
          out.addAll(blockIdToIn[succId]);
        }
        out.removeAll(blockIdToReadWrite[id].getSecond());
        Set<Local> in = blockIdToIn[id];
        if (in.addAll(out)) {
          changed = true;
        }
      }
    } while (changed);
    return blockIdToIn;
  }

  private static List<Local> getRetAndThis(BlockMethod blockMethod) {
    List<Local> result = new ArrayList<Local>();
    Iterator<Local> iLocals = blockMethod.getActiveBody().getLocals().iterator();
    result.add(iLocals.next());
    if (!blockMethod.isStatic()) {
      result.add(iLocals.next());
    }
    return result;
  }

  /**
   * Retrieve the set of variables that are read (without being
   * previously written by it) and written by a block method.
   *
   * @param blockMethod A block method in the iCFG.
   * @return Pair containing the two sets described, which can be understood as
   *         the "use" and "def" of the dataflow analysis literature.
   */
  private static Pair<Set<Local>, Set<Local>> getReadWrite(BlockMethod blockMethod) {
    Set<Local> read = new TreeSet<Local>(SootUtil.LOCAL_COMPARATOR);
    Set<Local> written = new TreeSet<Local>(SootUtil.LOCAL_COMPARATOR);
    Collection<Unit> units = blockMethod.getActiveBody().getUnits();
    for (Unit unit : units) {
      InvokeStmt invokeStmt = (InvokeStmt) unit;
      List<Immediate> args = invokeStmt.getInvokeExpr().getArgs();
      for (int i = 1; i < args.size(); i++) {
        Immediate apar = args.get(i);
        if (apar instanceof Local && !written.contains(apar)) {
          Local local = (Local) apar;
          if (!local.getType().equals(VoidType.v())) {
            read.add(local);
          }
        }
      }
      Immediate res = args.get(0);
      if (res instanceof Local && !res.getType().equals(VoidType.v())) {
        written.add((Local) res);
      }
    }
    return new Pair(read, written);
  }
}

