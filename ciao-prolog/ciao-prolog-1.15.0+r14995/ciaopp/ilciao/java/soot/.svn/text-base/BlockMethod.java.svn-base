package soot;

import soot.jimple.IfStmt;
import soot.jimple.toolkits.ciao.IcfgScene;
import soot.jimple.toolkits.ciao.Namer;
import soot.tagkit.LineNumberTag;
import soot.toolkits.graph.Block;
import soot.util.Chain;
import soot.util.SootUtil;

import java.util.ArrayList;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

public class BlockMethod extends SootMethod {

  public enum BlockMethodType {
    ENTRY, INTERNAL, PROXY, LIBRARY
  }

  private BlockMethodType type;
  private List<Local> formalParameters;
  /**
   * Corresponding block in the CFG, if any.
   */
  private Block block;
  /**
   * Corresponding Method in the program. Always non-null, even for
   * conditional proxies.
   */
  private SootMethod method;
  /**
   * Immediate predecessors in the iCFG.
   */
  private SortedSet<BlockMethod> preds;
  /**
   * Immediate successors in the iCFG.
   */
  private SortedSet<BlockMethod> succs;
  /**
   * BlockMethods with the same signature, only non-empy if the block method is
   * conditional or proxy.
   */
  private SortedSet<BlockMethod> siblings;
  /**
   * Guard, if any.
   */
  private IfStmt guard;

  public BlockMethod(String name, SootMethod method, List parameterTypes, Type returnType,
      int modifiers, int indexInMethod) {
    super(name, parameterTypes, returnType, modifiers);
    setModifiers(modifiers & ~Modifier.ABSTRACT); //all block methods are concrete
    this.method = method;
    formalParameters = new ArrayList<Local>();
    guard = null;
    succs = new TreeSet<BlockMethod>(SootUtil.BLOCK_METHOD_COMPARATOR);
    preds = new TreeSet<BlockMethod>(SootUtil.BLOCK_METHOD_COMPARATOR);
    siblings = new TreeSet<BlockMethod>(SootUtil.BLOCK_METHOD_COMPARATOR);
  }

  public static String getNameFor(SootMethod method, int indexInMethod) {
    String methodName = Namer.getEntryBlockMethodName(method);
    methodName += "#" + indexInMethod;
    methodName = method.getDeclaringClass().getName() + "." + methodName;
    return SootUtil.addQuotes(methodName);
  }

  public String getBlockMethodName() {
    SootClass sootClass = getDeclaringClass();
    StringBuffer blockMethodName = new StringBuffer(sootClass.getName());
    SootMethod method = getMethod();
    blockMethodName.append(".");
    blockMethodName.append(Namer.getEntryBlockMethodName(method));
    SortedSet<BlockMethod> siblingsAndMe = new TreeSet(SootUtil.BLOCK_METHOD_COMPARATOR);
    siblingsAndMe.addAll(getSiblings());
    siblingsAndMe.add(this);
    for (BlockMethod bm : siblingsAndMe) {
      blockMethodName.append("#");
      blockMethodName.append(bm.getIndexInMethod());
    }
    return SootUtil.addQuotes(blockMethodName.toString());
  }

  public boolean isProxy() {
    return type == BlockMethodType.PROXY;
  }

  public boolean isInternal() {
    return type == BlockMethodType.INTERNAL;
  }

  public boolean isEntry() {
    return (type == BlockMethodType.ENTRY || type == BlockMethodType.LIBRARY);
  }

  public boolean isLibrary() {
    return type == BlockMethodType.LIBRARY;
  }

  public boolean isConditional() {
    return getGuard() != null;
  }

  public void setType(BlockMethodType type) {
    this.type = type;
  }

  public BlockMethodType getType() {
    return type;
  }

  public List<Local> getFormalParameters() {
    if (formalParameters.isEmpty() && !isInternal()) {
      int numFormalParameters = getParameterCount();
      Chain chain = getActiveBody().getLocals();
      List<Local> locals = new ArrayList(chain);
      setFormalParameters(locals.subList(0, numFormalParameters));
    }
    return formalParameters;
  }

  public void setFormalParameters(List<Local> formalParameters) {
    this.formalParameters = formalParameters;
  }

  /**
   * The number of parameters of a blockMethod difers from the number
   * of parameters of the corresponding method, if any.
   *
   * @return Number of formal parameters of the block method.
   */
  public int getFormalParameterCount() {
    return isLibrary() ? getParameterCount() : getFormalParameters().size();
  }

  public Type getFormalParameterType(int i) {
    if (isLibrary()) {
      return getParameterType(i);
    } else {
      Local formalParameter = getFormalParameters().get(i);
      return formalParameter.getType();
    }
  }

  public List<Type> getFormalParameterTypes() {
    if (isLibrary()) {
      return getParameterTypes();
    } else {
      List<Type> result = new ArrayList<Type>();
      for (Local formalParameter : getFormalParameters()) {
        result.add(formalParameter.getType());
      }
      return result;
    }
  }

  public SortedSet<BlockMethod> getPreds() {
    return preds;
  }

  public void setPreds(SortedSet<BlockMethod> preds) {
    this.preds = preds;
  }

  public SortedSet<BlockMethod> getSuccs() {
    return succs;
  }

  public void addSucc(BlockMethod succ) {
    succs.add(succ);
  }

  public void setSuccs(SortedSet<BlockMethod> succs) {
    this.succs = succs;
  }

  public void addSibling(BlockMethod sibling) {
    siblings.add(sibling);
  }

  public SortedSet<BlockMethod> getSiblings() {
    return siblings;
  }

  public void setSiblings(SortedSet<BlockMethod> siblings) {
    this.siblings = siblings;
  }

  public int getIndexInMethod() {
    List<BlockMethod> blockMethodList = getBlockMethodList();
    int result = blockMethodList.indexOf(this);
    if (result == -1) {  //not present, add at the end
      result = blockMethodList.size();
    }
    return result;
  }

  public IfStmt getGuard() {
    return guard;
  }

  public void setGuard(IfStmt guard) {
    this.guard = guard;
  }

  public Block getBlock() {
    return block;
  }

  public void setBlock(Block block) {
    this.block = block;
  }

  public SootMethod getMethod() {
    return method;
  }

  public void setMethod(SootMethod method) {
    this.method = method;
  }

  public List<BlockMethod> getBlockMethodList() {
    String entryBlockMethodName = getNameFor(method, 0);
    return IcfgScene.getBlockMethodListFromEntryBlockMethodName(entryBlockMethodName);
  }

  public int getStartingLineInSource() {
    int lineNumber = -1;
    SootMethod method = getMethod();
    LineNumberTag lineNumberTag = (LineNumberTag) method.getTag("LineNumberTag");
    if (lineNumberTag != null) {
      lineNumber = lineNumberTag.getLineNumber();
      if (SootUtil.isConstructor(method)) {
        lineNumber++;
      }
    }
    return lineNumber;
  }
}
