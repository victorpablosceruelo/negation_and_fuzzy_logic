package soot.resources;

import soot.BlockMethod;
import soot.BooleanType;
import soot.ByteType;
import soot.CharType;
import soot.DoubleType;
import soot.FloatType;
import soot.Immediate;
import soot.IntType;
import soot.LongType;
import soot.RefType;
import soot.Scene;
import soot.ShortType;
import soot.SootClass;
import soot.SootField;
import soot.Type;
import soot.jimple.InvokeStmt;
import soot.jimple.StringConstant;
import soot.jimple.toolkits.ciao.Builtin;
import soot.util.SootUtil;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class HeapUsageFunction extends GenericResourceFunction {

  HeapUsageFunction() {
    super(Resource.HEAP_USAGE);
  }

  @Override
  public String getCost(InvokeStmt invokeStmt) {
    BlockMethod blockMethod = (BlockMethod) invokeStmt.getInvokeExpr().getMethod();
    if (SootUtil.isBuiltin(blockMethod) &&
        blockMethod.getBlockMethodName().contains(Builtin.NEWB.getName())) {
      List<Immediate> actualParameters = invokeStmt.getInvokeExpr().getArgs();
      String className = ((StringConstant) actualParameters.get(1)).value;
      SootClass sootClass = Scene.v().getSootClass(className);
      int heapUsage = getHeapUsage(sootClass.getType());
      return cost(heapUsage);
    } else {
      return cost(0);
    }
  }

  private static Map<Type, Integer> typeToSizeMap = new HashMap<Type, Integer>();

  private int getHeapUsage(Type type) {
    Integer size = typeToSizeMap.get(type);
    if (size == null) {
      size = getHeapUsage_(type);
      typeToSizeMap.put(type, size);
    }
    return size;
  }

  //TODO : I couldn't figure out the size of an object in Java (like a String)
  // For now, add 4 bytes per object
  /**
   * For every data type in the language, return an upper bound on its size (in bytes).
   * Data for primitive types have been extracted from
   * http://java.sun.com/docs/books/tutorial/java/nutsandbolts/datatypes.html
   *
   * @param type Type (reference or primitive).
   * @return Upper bound in the number of bytes used to represent the given type.
   */
  private int getHeapUsage_(Type type) {
    int result = 0;
    if (type instanceof RefType) {
      result += 4;
      RefType refType = (RefType) type;
      SootClass sootClass = refType.getSootClass();
      List<SootField> sootFields = new ArrayList(sootClass.getFields());
      for (SootField sootField : sootFields) {
        Type fieldType = sootField.getType();
        result += getHeapUsage(fieldType);
      }
    } else if (type instanceof BooleanType) {
      return 1;
    } else if (type instanceof ByteType) {
      return 1;
    } else if (type instanceof CharType) {
      return 2;
    } else if (type instanceof DoubleType) {
      return 8;
    } else if (type instanceof FloatType) {
      return 4;
    } else if (type instanceof IntType) {
      return 4;
    } else if (type instanceof LongType) {
      return 8;
    } else if (type instanceof ShortType) {
      return 2;
    } else {
      throw new RuntimeException("Heap usage not implement yet for type " + type);
    }
    return result;
  }

}
