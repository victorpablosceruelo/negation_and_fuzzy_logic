package soot.jimple.toolkits.ciao;

import soot.BlockMethod;
import soot.Body;
import soot.G;
import soot.Modifier;
import soot.Scene;
import soot.SootClass;
import soot.SootMethod;
import soot.Type;
import soot.Unit;
import soot.Value;
import soot.dava.toolkits.base.misc.ConditionFlipper;
import soot.jimple.AssignStmt;
import soot.jimple.ConditionExpr;
import soot.jimple.IfStmt;
import soot.jimple.InstanceFieldRef;
import soot.jimple.Jimple;
import soot.options.CiaoOptions;
import soot.shimple.Shimple;
import soot.shimple.ShimpleBody;
import soot.tagkit.Host;
import soot.tagkit.LineNumberTag;
import soot.toolkits.graph.Block;
import soot.toolkits.graph.BlockGraph;
import soot.toolkits.graph.ExceptionalBlockGraph;
import soot.util.Chain;
import soot.util.SootUtil;
import util.Pair;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

public class IcfgScene {

  private static IcfgScene instance;
  private static SortedSet<SootClass> classes;
  private static Map<String, List<BlockMethod>> methodMap;
  private static Set<Block>[] blocksSiblings;


  private IcfgScene() {
    classes = new TreeSet<SootClass>(SootUtil.CLASS_COMPARATOR);
    methodMap = new HashMap<String, List<BlockMethod>>();
  }

  public static IcfgScene v() {
    if (instance == null) {
      instance = new IcfgScene();
    }
    return instance;
  }

  protected void createScene() {
    List<SootClass> appClasses = new ArrayList(Scene.v().getApplicationClasses());
    for (SootClass appClass : appClasses) {
      for (SootMethod method : (List<SootMethod>) appClass.getMethods()) {
        if (method.isConcrete()) {
          Body methodBody = method.getActiveBody();
          Body newMethodBody = encodeSetfieldsAsAssignments(methodBody);
          getBlockSiblings(newMethodBody);
          List<BlockMethod> blockMethods = getEmptyBlockMethods(newMethodBody);
          //SootUtil.printBlockMethodRelationships(blockMethods);
        } else if (method.isNative()) {
          newLibraryBlockMethod(method);
        }
      }
    }
    G.v().out.println("\n\n");
  }

  /**
   * Assignment to fields x.f=v might change the properties of a variable
   * (e.g.: maximum number of pointer until hitting null). A way of expressing
   * the modification is by assigning a value to x, so the SSA conversion
   * will create a new version of the variable. The chosen assignment is in
   * the form x=x.f; a tag with the value v is attached so we can recognize
   * the assignment as a setfield and recover the right hand-side.
   *
   * @param body Body to transform, in non-SSA form.
   * @return New body in SSA form in which setfields have been replaced by
   *         getfields, and properly marked.
   */
  private Body encodeSetfieldsAsAssignments(Body body) {
    Body newBody = body;
    if (CiaoOptions.v().isResourcesMode()) {
      Chain units = body.getUnits();
      Iterator<Unit> unitIterator = units.snapshotIterator();
      while (unitIterator.hasNext()) {
        Unit unit = unitIterator.next();
        if (unit instanceof AssignStmt) {
          AssignStmt assignStmt = (AssignStmt) unit;
          Value lhs = assignStmt.getLeftOp();
          if (lhs instanceof InstanceFieldRef) { // is a setfield
            InstanceFieldRef instanceFieldRef = (InstanceFieldRef) assignStmt.getLeftOp();
            Value base = instanceFieldRef.getBase();
            Unit newUnit = Jimple.v().newAssignStmt(base, instanceFieldRef);
            newUnit.addTag(new SetfieldTag(assignStmt.getRightOp()));
            // we do NOT delete the original x.f=v since it might be the only
            // reference to v in the code. The purpose is to prevent v from being
            // deleted in the process Jimple-Shimple-Jimple.
            units.insertBefore(newUnit, unit);
          }
        }
      }
      ShimpleBody sBody = Shimple.v().newBody(body);
      newBody = Shimple.v().newJimpleBody(sBody);
      SootMethod method = body.getMethod();
      method.setActiveBody(newBody);
    }
    return newBody;
  }

  public static BlockMethod newLibraryBlockMethod(SootMethod methodInvoked) {
    String libraryMethodName = Namer.getFullBlockMethodNameForMethod(methodInvoked);
    if (IcfgScene.containsEntryBlockMethod(libraryMethodName)) {
      return IcfgScene.getEntryBlockMethod(libraryMethodName);
    } else {
      List<Type> formalParameters = SootUtil.getParameterTypesOfLibraryMethod(methodInvoked);
      BlockMethod libraryMethod =
          newBlockMethod(methodInvoked, formalParameters, BlockMethod.BlockMethodType.LIBRARY, 0);
      IcfgScene.addBlockMethod(libraryMethodName, libraryMethod);
      return libraryMethod;
    }
  }

  private void getBlockSiblings(Body body) {
    ExceptionalBlockGraph blockGraph = new ExceptionalBlockGraph(body);
    List<Block> blocks = blockGraph.getBlocks();
    int numBlocks = blocks.size();
    Set<Block>[] result = new Set[numBlocks];
    for (Block block : blocks) {
      Set<Block> siblings = new HashSet<Block>();
      int id = block.getIndexInMethod();
      List<Block> preds = block.getPreds();
      for (Block pred : preds) {
        List<Block> succs = pred.getSuccs();
        for (Block sibling : succs) {
          int siblingId = sibling.getIndexInMethod();
          if (siblingId != id) {
            siblings.add(sibling);
          }
        }
      }
      result[id] = siblings;
    }
    blocksSiblings = result;
  }

  private List<BlockMethod> getEmptyBlockMethods(Body body) {
    ExceptionalBlockGraph blockGraph = new ExceptionalBlockGraph(body);
    List<Block> blocks = blockGraph.getBlocks();
    List<BlockMethod> blockMethods = new ArrayList<BlockMethod>();
    SootMethod method = blocks.get(0).getBody().getMethod();
    String entryMethodName = Namer.getFullBlockMethodNameForMethod(method);
    for (Block block : blocks) {
      BlockMethod blockMethod = createBlockMethodFromBlock(block);
      blockMethods.add(blockMethod);
    }
    methodMap.put(entryMethodName, blockMethods);
    addBlockPropertiesToBlockMethod(blocks, blockMethods);
    System.out.println("---------------- Original ICFG ----------------");
    SootUtil.printBlockMethodRelationships(blockMethods);
    processBlockMethodsWithMoreThanOneSibling(blockMethods);
    System.out
        .println("---------------- After removal of BMs with more than 1 sibling ----------------");
    SootUtil.printBlockMethodRelationships(blockMethods);
    processIfThenElseBlockMethods(blocks, blockMethods, blockGraph);
    return blockMethods;
  }

  private void addBlockPropertiesToBlockMethod(List<Block> blocks, List<BlockMethod> blockMethods) {
    for (Block block : blocks) {
      BlockMethod blockMethod = getBlockMethodFromBlock(block);
      int indexInMethod = block.getIndexInMethod();
      List<Block> preds = block.getPreds();
      blockMethod.setPreds(getSortedView(blockMethods, preds));
      List<Block> succs = block.getSuccs();
      Set<Block> siblings = blocksSiblings[indexInMethod];
      blockMethod.setSiblings(getSortedView(blockMethods, siblings));
      blockMethod.setSuccs(getSortedView(blockMethods, succs));
      blockMethod.setBlock(block);
      blockMethod.setMethod(block.getBody().getMethod());
      if (SootUtil.isBuiltin(blockMethod)) {
        blockMethod.setType(BlockMethod.BlockMethodType.LIBRARY);
      }
    }
  }

  private SortedSet<BlockMethod> getSortedView(List<BlockMethod> blockMethods,
      Collection<Block> blockRelatives) {
    SortedSet<BlockMethod> result = new TreeSet<BlockMethod>(SootUtil.BLOCK_METHOD_COMPARATOR);
    for (Block relative : blockRelatives) {
      int indexInMethod = relative.getIndexInMethod();
      result.add(blockMethods.get(indexInMethod));
    }
    return result;
  }

  /**
   * When a blockMethod bm has more than one sibling, and they are not at
   * the same level in the CFG, they cannot all have the same name.
   * We solve the problem by adding a new block method for each sibling
   * of bm, which will be empty and have bm as unique successor.
   *
   * @param blockMethods List of blockMethods in the original iCFG.
   */
  private void processBlockMethodsWithMoreThanOneSibling(List<BlockMethod> blockMethods) {
    Collection<BlockMethod> result = new ArrayList<BlockMethod>();
    // We cannot use an Iterator here, since we are adding elements
    // to blockMethods at the same time.
    for (int i = 0; i < blockMethods.size(); i++) {
      BlockMethod blockMethod = blockMethods.get(i);
      Set<BlockMethod> siblings = blockMethod.getSiblings();
      int numSiblings = siblings.size();
      if (numSiblings > 1) {
        blockMethod.getPreds().clear();
        for (BlockMethod sibling : siblings) {
          int numBlockMethods = blockMethods.size();
          BlockMethod proxy = newBlockMethod(sibling.getMethod(), sibling.getParameterTypes(),
              BlockMethod.BlockMethodType.INTERNAL, numBlockMethods);
          sibling.getSiblings().remove(blockMethod);
          sibling.getSiblings().add(proxy);
          proxy.getSiblings().add(sibling);
          proxy.getSuccs().add(blockMethod);
          for (BlockMethod pred : sibling.getPreds()) {
            pred.getSuccs().remove(blockMethod);
            pred.getSuccs().add(proxy);
            proxy.getPreds().add(pred);
          }
          blockMethod.getPreds().add(proxy);
          result.add(proxy);
          blockMethods.add(proxy);
        }
        blockMethod.getSiblings().clear();
      }
    }
  }

  /**
   * If a blockMethod happens to be an if or else block (destination of a if or else bytecode)
   * , tag it and add the guard. The identification of a blockMethod
   * as if or else depends on the head of the corresponding block, and therefore it
   * should be done before any transformation on it.
   *
   * @param blocks List of Blocks in the CGF.
   */
  private void processIfThenElseBlockMethods(List<Block> blocks, List<BlockMethod> blockMethods,
      BlockGraph blockGraph) {
    List<BlockMethod> result = new ArrayList<BlockMethod>();
    for (Block block : blocks) {
      int numBlockMethods = blockMethods.size() + result.size();
      Unit tail = block.getTail();
      if (tail instanceof IfStmt) {
        IfStmt ifStmt = (IfStmt) tail;
        BlockMethod ifBlockMethod, elseBlockMethod;
        Block firstSucc = (Block) block.getSuccs().get(0);
        Block secondSucc;
        if (block.getSuccs().size() == 2) {
          secondSucc = (Block) block.getSuccs().get(1);
          Pair<BlockMethod, BlockMethod> ifAndElseBlockMethods =
              getIfAndElseBlockMethods(ifStmt, block, firstSucc, secondSucc, numBlockMethods);
          ifBlockMethod = ifAndElseBlockMethods.getFirst();
          elseBlockMethod = ifAndElseBlockMethods.getSecond();
        } else {
          ifBlockMethod = getIfBlockMethod(firstSucc);
          secondSucc = createEmptyBlock(firstSucc, blockGraph, numBlockMethods);
          elseBlockMethod = createBlockMethodFromBlock(secondSucc);
          elseBlockMethod.setType(BlockMethod.BlockMethodType.PROXY);
          SortedSet<BlockMethod> preds = ifBlockMethod.getPreds();
          for (BlockMethod pred : preds) {
            pred.addSucc(elseBlockMethod);
          }
          elseBlockMethod.setPreds(preds);
          ifBlockMethod.addSibling(elseBlockMethod);
          elseBlockMethod.addSibling(ifBlockMethod);
        }
        configureIfThenElseBlockMethods(ifStmt, ifBlockMethod, elseBlockMethod);
        if (ifBlockMethod.isProxy()) {
          result.add(ifBlockMethod);
        }
        if (elseBlockMethod.isProxy()) {
          result.add(elseBlockMethod);
        }
      }
    }
    blockMethods.addAll(result);
  }

  private static Block createEmptyBlock(Block sibling, BlockGraph blockGraph, int index) {
    Unit nop = Jimple.v().newNopStmt();
    Body body = sibling.getBody();
    return new Block(nop, nop, body, index, 1, blockGraph);
  }

  private static BlockMethod getIfBlockMethod(Block firstSucc) {
    return getBlockMethodFromBlock(firstSucc);
  }

  private static Pair<BlockMethod, BlockMethod> getIfAndElseBlockMethods(IfStmt ifStmt,
      Block predBlock, Block firstSucc, Block secondSucc, int numBlocks) {
    boolean firstSuccIsIf = ifStmt.getTarget().equals(firstSucc.getHead());
    Block ifBlock = firstSuccIsIf ? firstSucc : secondSucc;
    Block elseBlock = firstSuccIsIf ? secondSucc : firstSucc;
    BlockMethod ifBlockMethod = getBlockMethodFromBlock(ifBlock);
    BlockMethod elseBlockMethod = getBlockMethodFromBlock(elseBlock);
    if (firstSucc.getPreds().size() + secondSucc.getPreds().size() > 2) {
      BlockMethod pred = getBlockMethodFromBlock(predBlock);
      BlockMethod proxyIfBlockMethod = newProxyBlockMethod(pred, ifBlockMethod, numBlocks);
      BlockMethod proxyElseBlockMethod = newProxyBlockMethod(pred, elseBlockMethod, numBlocks + 1);
      proxyIfBlockMethod.addSibling(proxyElseBlockMethod);
      proxyElseBlockMethod.addSibling(proxyIfBlockMethod);
      ifBlockMethod = proxyIfBlockMethod;
      elseBlockMethod = proxyElseBlockMethod;
    }
    return new Pair(ifBlockMethod, elseBlockMethod);
  }

  private void configureIfThenElseBlockMethods(IfStmt ifStmt, BlockMethod ifBlockMethod,
      BlockMethod elseBlockMethod) {
    Unit nop = Jimple.v().newNopStmt();
    ConditionExpr conditionExpr = (ConditionExpr) ifStmt.getCondition();
    IfStmt guardStmt = Jimple.v().newIfStmt(conditionExpr, nop);
    ifBlockMethod.setGuard(guardStmt);
    Unit elseNop = Jimple.v().newNopStmt();
    ConditionExpr elseConditionExpr = (ConditionExpr) ifStmt.getCondition().clone();
    elseConditionExpr = ConditionFlipper.flip(elseConditionExpr);
    IfStmt elseGuardStmt = Jimple.v().newIfStmt(elseConditionExpr, elseNop);
    elseBlockMethod.setGuard(elseGuardStmt);
  }

  private static BlockMethod newProxyBlockMethod(BlockMethod pred, BlockMethod blockMethod,
      int index) {
    List<Type> parameterTypes = blockMethod.getParameterTypes();
    BlockMethod proxy = newBlockMethod(blockMethod.getMethod(), parameterTypes,
        BlockMethod.BlockMethodType.PROXY, index);
    proxy.setMethod(pred.getMethod());
    pred.getSuccs().remove(blockMethod);
    pred.getSuccs().add(proxy);
    blockMethod.getPreds().remove(pred);
    blockMethod.getPreds().add(proxy);
    blockMethod.getSiblings().clear();
    proxy.getPreds().add(pred);
    proxy.getSuccs().add(blockMethod);
    return proxy;
  }

  private static BlockMethod createBlockMethodFromBlock(Block block) {
    SootMethod originalMethod = block.getBody().getMethod();
    List<Type> paramTypes = SootUtil.getParameterTypesOfLibraryMethod(originalMethod);
    BlockMethod.BlockMethodType blockMethodType;
    if (IcfgGenerator.isEntryBlock(block)) {
      blockMethodType = BlockMethod.BlockMethodType.ENTRY;
    } else {
      blockMethodType = BlockMethod.BlockMethodType.INTERNAL;
    }
    int indexInMethod = block.getIndexInMethod();
    BlockMethod blockMethod =
        newBlockMethod(originalMethod, paramTypes, blockMethodType, indexInMethod);
    setLineNumberTag(blockMethod, originalMethod, block);
    return blockMethod;
  }

  public static BlockMethod newBlockMethod(SootMethod method, List<Type> paramTypes,
      BlockMethod.BlockMethodType blockMethodType, int indexInMethod) {
    Type returnType = method.getReturnType();
    int modifiers = method.getModifiers();
    String methodName = BlockMethod.getNameFor(method, indexInMethod);
    BlockMethod blockMethod =
        new BlockMethod(methodName, method, paramTypes, returnType, modifiers, indexInMethod);
    blockMethod.setType(blockMethodType);
    if (blockMethodType != BlockMethod.BlockMethodType.ENTRY) {
      blockMethod.setModifiers(blockMethod.getModifiers() | Modifier.PRIVATE);
    }
    blockMethod.addAllTagsOf(method);
    SootClass cloneClass = getCloneOfSootClass(method.getDeclaringClass());
    cloneClass.addMethod(blockMethod);
    return blockMethod;
  }

  private static void setLineNumberTag(BlockMethod blockMethod, SootMethod originalMethod,
      Block block) {
    Host from = blockMethod.isEntry() ? originalMethod : block.getHead();
    adjustLineNumberWhenConstructor(from, originalMethod, blockMethod);
  }

  private static void adjustLineNumberWhenConstructor(Host from, SootMethod originalMethod,
      BlockMethod blockMethod) {
    LineNumberTag lineNumberTag = (LineNumberTag) from.getTag("LineNumberTag");
    if (lineNumberTag != null) {
      if (SootUtil.isConstructor(originalMethod)) {
        int newLineNumber = lineNumberTag.getLineNumber();
        lineNumberTag = new LineNumberTag(newLineNumber + 1);
      }
      blockMethod.removeTag("LineNumberTag");
      blockMethod.addTag(lineNumberTag);
    }
  }

  private static SootClass getCloneOfSootClass(SootClass originalClass) {
    if (!classes.contains(originalClass)) {
      SootClass cloneClass = cloneSootClass(originalClass);
      classes.add(cloneClass);
      return cloneClass;
    } else { //complicated way of retrieving an element from a set
      return classes.tailSet(originalClass).first();
    }
  }

  /**
   * For every class in the CFG that contains some methods, we create
   * a clone that will contain only the corresponding block methods.
   *
   * @param originalClass Class in the CFG.
   * @return Another class with the same attributes but no methods on it.
   */
  private static SootClass cloneSootClass(SootClass originalClass) {
    SootClass cloneClass = new SootClass(originalClass.getName(), originalClass.getModifiers());
    if (!originalClass.getName().equals("java.lang.Object")) {
      cloneClass.setSuperclass(originalClass.getSuperclass());
    }
    return cloneClass;
  }

  public static Map<String, List<BlockMethod>> getMethodMap() {
    return methodMap;
  }

  public static boolean containsEntryBlockMethod(String entryBlockMethodName) {
    return methodMap.containsKey(entryBlockMethodName);
  }

  public static BlockMethod getEntryBlockMethod(String entryBlockMethodName) {
    List<BlockMethod> blockMethods =
        getBlockMethodListFromEntryBlockMethodName(entryBlockMethodName);
    return blockMethods.get(0);
  }

  public static List<BlockMethod> getBlockMethodListFromEntryBlockMethodName(
      String entryBlockMethodName) {
    List<BlockMethod> blockMethods = methodMap.get(entryBlockMethodName);
    if (blockMethods == null) {
      throw new RuntimeException("Entry block method " + entryBlockMethodName +
          " not found in map. Maybe you mispelled the name?");
    } else if (blockMethods.isEmpty()) {
      throw new RuntimeException("Inconsistency detected: entry block method " +
          entryBlockMethodName + " is in the map, but with no block methods associated.");
    }
    return blockMethods;
  }

  public static void addBlockMethod(String entryBlockMethodName, BlockMethod blockMethod) {
    List<BlockMethod> blockMethods = methodMap.get(entryBlockMethodName);
    if (blockMethods == null) {
      blockMethods = new ArrayList<BlockMethod>();
      methodMap.put(entryBlockMethodName, blockMethods);
    }
    blockMethods.add(blockMethod);
  }

  public static BlockMethod getBlockMethodFromBlock(Block block) {
    SootMethod method = block.getBody().getMethod();
    String entryBlockName = Namer.getFullBlockMethodNameForMethod(method);
    List<BlockMethod> blockMethodList = getBlockMethodListFromEntryBlockMethodName(entryBlockName);
    int index = block.getIndexInMethod();
    if (index + 1 > blockMethodList.size()) {
      throw new IndexOutOfBoundsException("Referenced block method #" + index +
          ", although entry block " + "method " + entryBlockName + " has only " +
          blockMethodList.size() + " block methods" + " associated.");
    }
    BlockMethod result = blockMethodList.get(index);
    if (result == null) {
      throw new RuntimeException(
          "Internal block method #" + index + " not found, although the entry block is there.");
    }
    return result;
  }

  public static List<BlockMethod> getLibraryBlockMethods() {
    List<BlockMethod> libraryMethods = new ArrayList<BlockMethod>();
    for (String key : methodMap.keySet()) {
      BlockMethod first = getEntryBlockMethod(key);
      if (first.isLibrary()) {
        libraryMethods.add(first);
      }
    }
    return libraryMethods;
  }

  /**
   * Given a method, retrieve the corresponding entry block method in the iCFG.
   * Before any optimization, that block method should be in the first position of the list.
   *
   * @param method A method present in the CFG.
   * @return The entry block method that resulted from the transformation of the method.
   */
  public static BlockMethod getEntryBlockMethodFromMethod(SootMethod method) {
    String entryBlockName = Namer.getFullBlockMethodNameForMethod(method);
    if (containsEntryBlockMethod(entryBlockName)) {
      return getEntryBlockMethod(entryBlockName);
    } else {
      throw new RuntimeException("Block method not found for method " + method);
    }
  }

  public static BlockMethod getBuiltinMethod(Builtin builtin) {
    Set<String> keySet = methodMap.keySet();
    for (String key : keySet) {
      BlockMethod first = getEntryBlockMethod(key);
      if (first.getMethod().getName().equals(builtin.getName())) {
        return first;
      }
    }
    throw new RuntimeException("No block method found for builtin " + builtin.getName());
  }
}
