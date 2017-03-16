package soot.jimple.toolkits.ciao;

import soot.BlockMethod;
import soot.Local;
import soot.RefType;
import soot.SootClass;
import soot.SootMethod;
import soot.Type;
import soot.Value;
import soot.jimple.StringConstant;
import soot.util.SootUtil;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

public class Namer {

  public static final String RET = "ret";
  public static final String THIS = "this";

  public static String getEntryBlockMethodName(SootMethod method) {
    SootClass theClass = method.getDeclaringClass();
    int index = theClass.getMethods().indexOf(method);
    return method.getName() + "." + index;
  }

  /**
   * Returns the iCFG name of a Soot method or the block name of a block method.
   *
   * @param method Method in the CFG, or library method invoked from the iCFG, or
   *               block method in the iCFG.
   * @return Name of the method congruent with the standard naming.
   */
  static String getFullBlockMethodNameForMethod(SootMethod method) {
    if (method instanceof BlockMethod) {
      return ((BlockMethod) method).getBlockMethodName();
    } else {
      return BlockMethod.getNameFor(method, 0);
    }
  }

  static String getFullBlockMethodNameAndArity(BlockMethod blockMethod) {
    return blockMethod.getBlockMethodName() + "/" + blockMethod.getFormalParameterCount();
  }

  public static String printType(Type type) {
    if (type instanceof RefType) {
      RefType refType = (RefType) type;
      SootClass sootClass = refType.getSootClass();
      return sootClass.getName();
    } else {
      return type.toString();
    }
  }

  static String printQuotedTypes(Set<Type> types) {
    List<String> lResult = new ArrayList<String>();
    for (Type type : types) {
      lResult.add(SootUtil.addQuotes(printType(type)));
    }
    return lResult.toString();
  }

  public static String toProlog(String javaVar) {
    if (javaVar.equals("void")) {
      return javaVar;
    } else {
      if (javaVar.startsWith("$")) {
        javaVar = javaVar.substring(1);
      }
      javaVar = javaVar.replace('$', '_');
      return Character.toUpperCase(javaVar.charAt(0)) + javaVar.substring(1);
    }
  }

  public static String toProlog(Value value) {
    if (value instanceof Local) {
      return toProlog(value.toString());

    } else if (value instanceof StringConstant) {
      StringConstant sc = (StringConstant) value;
      String stringValue = sc.value;
      if (SootUtil.isAlreadyQuoted(stringValue)) {
        int length = stringValue.length();
        stringValue = stringValue.substring(1, length - 1);
      }
      stringValue = stringValue.replace("'", "\\'");
      return SootUtil.addQuotes(stringValue);

    } else {
      return value.toString();
    }
  }
}
