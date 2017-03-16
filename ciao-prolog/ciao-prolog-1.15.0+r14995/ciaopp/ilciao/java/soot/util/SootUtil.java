package soot.util;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import soot.BlockMethod;
import soot.G;
import soot.Local;
import soot.SootClass;
import soot.SootMethod;
import soot.Type;
import soot.options.CiaoOptions;
import soot.options.Options;
import soot.tagkit.Host;
import soot.tagkit.LineNumberTag;
import soot.tagkit.Tag;
import util.Pair;

import java.io.BufferedOutputStream;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.lang.annotation.Annotation;
import java.lang.reflect.AnnotatedElement;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.List;
import java.util.Properties;

public class SootUtil {

  public static Properties getProperties(@NotNull String fileName) {
    Properties propertiesFile = new Properties();
    try {
      propertiesFile.load(new FileInputStream(fileName));
    } catch (IOException e) {
      throw new RuntimeException(e.getMessage());
    }
    return propertiesFile;
  }

  public static BufferedOutputStream openFileInOutputDir(@NotNull String fileName) {
    try {
      String outputDir = Options.v().output_dir();
      return new BufferedOutputStream(new FileOutputStream(outputDir + "/" + fileName));
    } catch (FileNotFoundException ex) {
      ex.printStackTrace();
      throw new RuntimeException("");
    }
  }

  public static void closeStream(OutputStream stream) {
    try {
      stream.close();
    } catch (IOException ex) {
      System.err.println("Error while trying to close stream. ");
      throw new RuntimeException(ex);
    }
  }

  public static void writeToOutput(OutputStream stream, @NotNull String ciao) {
    if (CiaoOptions.v().isVerboseIlCiao()) {
      G.v().out.print(ciao);
    }
    try {
      stream.write(ciao.getBytes());
    } catch (IOException ex) {
      System.err.println("Error while writing to file!");
      throw new RuntimeException(ex.getMessage());
    }
  }

  public static List<String> getListFromCsv(@NotNull String csv) {
    if (csv.matches("([.,]+[.] | [.]?)")) {
      throw new IllegalArgumentException(
          "The list " + csv + " seems not to be in standard format.");
    } else {
      int length = csv.length();
      if (length > 0) {
        String[] elements = csv.split(",");
        return Arrays.asList(elements);
      } else {
        return new ArrayList<String>();
      }
    }
  }

  public static String removeLastComma(@NotNull String string) {
    int length = string.length();
    if (length > 0) {
      string = string.trim();
      length = string.length();
      if (string.charAt(length - 1) == ',') {
        return string.substring(0, length - 1);
      }
    }
    return string;
  }

  public static String printListWithoutBraces(@NotNull List<String> list) {
    if (list.isEmpty()) {
      return "";
    } else {
      String lResult = list.toString();
      int length = lResult.length();
      return lResult.substring(1, length - 1);
    }
  }

  public static String listToStringTuple(@NotNull List<String> list) {
    if (list.isEmpty()) {
      return "";
    } else {
      String result = list.toString();
      int length = result.length();
      return "(" + result.substring(1, length - 1) + ")";
    }
  }

  public static String addQuotes(@NotNull String s) {
    return isAlreadyQuoted(s) ? s : "'" + s + "'";
  }

  public static boolean isAlreadyQuoted(@NotNull String s) {
    int length = s.length();
    return (length > 1 && s.charAt(0) == '\'' && s.charAt(length - 1) == '\'');
  }

  public static List<Type> getTypes(@NotNull Collection<Local> locals) {
    List<Type> result = new ArrayList<Type>();
    for (Local local : locals) {
      result.add(local.getType());
    }
    return result;
  }

  public static void printBlockMethodRelationships(@NotNull List<BlockMethod> blockMethods) {
    if (blockMethods.size() > 0) {
      System.out.println(blockMethods.get(0).getBlockMethodName());
      for (BlockMethod blockMethod : blockMethods) {
        int i = blockMethod.getIndexInMethod();
        System.out.print("\t\t BM " + i);
        System.out.print("\t\t\tsuccs = ");
        SootUtil.printBlockMethodsIndexes(blockMethod.getSuccs());
        System.out.print("\t\t\tsiblings = ");
        SootUtil.printBlockMethodsIndexes(blockMethod.getSiblings());
        System.out.print("\t\t\tpreds = ");
        SootUtil.printBlockMethodsIndexes(blockMethod.getPreds());
        System.out.println();
      }
    }
  }

  private static void printBlockMethodsIndexes(@NotNull Collection<BlockMethod> blockMethods) {
    for (BlockMethod blockMethod : blockMethods) {
      System.out.print(blockMethod.getIndexInMethod() + "-");
    }
  }

  /**
   * Number of parameters of iCFG block method for a given CFG method.
   *
   * @param method CFG method.
   * @return Number of parameters that the equivalent block method would have.
   */
  public static int getParameterCountOfLibraryMethod(@NotNull SootMethod method) {
    return getParameterTypesOfLibraryMethod(method).size();
  }

  public static List<Type> getParameterTypesOfLibraryMethod(@NotNull SootMethod method) {
    List<Type> result = new ArrayList<Type>();
    result.add(method.getReturnType());
    if (!method.isStatic()) {
      result.add(method.getDeclaringClass().getType());
    }
    result.addAll(method.getParameterTypes());
    return result;
  }

  public static boolean isBuiltin(SootMethod method) {
    return method.getDeclaringClass().getName().equals("lang.Builtin");
  }

  public static boolean isJdkType(Type type) {
    String typeName = type.toString();
    return isJdkType(typeName);
  }

  public static boolean isJdkType(String typeName) {
    return (typeName.startsWith("sun.") || typeName.startsWith("java.") ||
        typeName.startsWith("javax.") || typeName.startsWith("org.xml.sax") ||
        typeName.startsWith("org.w3c.dom") || typeName.startsWith("org.omg") ||
        typeName.startsWith("org.ietf"));
  }

  public static int getSourceLineNumber(Host host) {
    Tag tag = host.getTag("LineNumberTag");
    if (tag != null) {
      LineNumberTag lineNumberTag = (LineNumberTag) tag;
      return lineNumberTag.getLineNumber();
    } else {
      return -1;
    }
  }

  @Nullable
  public static Annotation getAnnotation(SootClass sootClass, Class annotationClass) {
    AnnotatedElement theClass = getJavaClass(sootClass);
    return theClass.getAnnotation(annotationClass);
  }

  @Nullable
  public static Annotation getAnnotation(BlockMethod blockMethod, Class annotationClass) {
//    List<Tag> tags = originalMethod.getTags();
//    for (Tag tag : tags) {
//      System.out.println(">>>>>>>>>>>>>>>>>>>>> FOUND TAG!!!!!!!!" + tag.getName());
//    }
    Annotation annotation = null;
    if (blockMethod.isEntry() && !blockMethod.getName().contains("<clinit>")) {
      Pair<AnnotatedElement, AnnotatedElement> classAndmethodOrConstructor =
          getJavaClassAndMethodOrConstructor(blockMethod);
      AnnotatedElement methodOrConstructor = classAndmethodOrConstructor.getSecond();
      annotation = methodOrConstructor.getAnnotation(annotationClass);
      if (annotation == null) {
        AnnotatedElement theClass = classAndmethodOrConstructor.getFirst();
        annotation = theClass.getAnnotation(annotationClass);
      }
    }
    return annotation;
  }

  @Nullable
  public static Annotation getAnnotation(BlockMethod blockMethod, int parameterNum,
      Class annotationClass) {
    if (blockMethod.isEntry() && !blockMethod.getName().contains("<clinit>")) {
      Pair<AnnotatedElement, AnnotatedElement> classAndmethodOrConstructor =
          getJavaClassAndMethodOrConstructor(blockMethod);
      AnnotatedElement methodOrConstructor = classAndmethodOrConstructor.getSecond();
      Annotation[][] annotations;
      if (methodOrConstructor instanceof Method) {
        annotations = ((Method) methodOrConstructor).getParameterAnnotations();
      } else {
        annotations = ((Constructor) methodOrConstructor).getParameterAnnotations();
      }
      if (annotations.length > 0) {
        for (Annotation annotation : annotations[parameterNum]) {
          if (annotation.annotationType().equals(annotationClass)) {
            return annotation;
          }
        }
      }
    }
    return null;
  }

  @NotNull
  private static Pair<AnnotatedElement, AnnotatedElement> getJavaClassAndMethodOrConstructor(
      BlockMethod blockMethod) {
    SootMethod originalMethodOrConstructor = blockMethod.getMethod();
    Class javaClass = getJavaClass(originalMethodOrConstructor);
    AnnotatedElement constructorOrMethod;
    if (isConstructor(originalMethodOrConstructor)) {
      constructorOrMethod = findConstructor(originalMethodOrConstructor, javaClass);
    } else {
      constructorOrMethod = findMethod(originalMethodOrConstructor, javaClass);
    }
    return new Pair(javaClass, constructorOrMethod);
  }

  @NotNull
  private static Class getJavaClass(SootMethod originalMethod) {
    return getJavaClass(originalMethod.getDeclaringClass());
  }

  @NotNull
  private static Class getJavaClass(SootClass sootClass) {
    Class theClass;
    String className = sootClass.getName();
    try {
      if (className.startsWith("java.")) {
        className = className.replaceFirst("java", "annot_java");
      }
      theClass = Class.forName(className);
    } catch (ClassNotFoundException cnfe1) {
      try {
        if (className.startsWith("annot_java.")) {
          className = className.replaceFirst("annot_java", "java");
        }
        theClass = Class.forName(className);
      } catch (ClassNotFoundException cnfe2) {
        throw new RuntimeException("Java class: " + className + "not found.");
      }
    }
    return theClass;
  }

  /**
   * Checks whether a method is a constructor.
   *
   * @param method Method in the CFG or Block Method in the iCFG.
   * @return True if the method is a constructor or the block
   *         method results from the compilation of a constructor.
   */
  public static boolean isConstructor(SootMethod method) {
    return method.getName().contains("<init>");
  }

  @NotNull
  private static AnnotatedElement findConstructor(SootMethod originalMethod, Class javaClass) {
    int numMethodParams = originalMethod.getParameterCount();
    for (Constructor constructor : javaClass.getDeclaredConstructors()) {
      int numConstructorParams = constructor.getParameterTypes().length;
      if (numMethodParams == numConstructorParams) {
        return constructor;
      }
    }
    throw new RuntimeException("No constructor with signature " + originalMethod +
        " found in class " + javaClass.getName());
  }

  @NotNull
  private static AnnotatedElement findMethod(SootMethod originalMethod, Class javaClass) {
    String originalMethodName = originalMethod.getName();
    for (Method method : javaClass.getDeclaredMethods()) {
      if (method.getName().equals(originalMethodName)) {
        return method;
      }
    }
    String className = javaClass.getName();
    throw new RuntimeException("No method with name " + originalMethodName + " found in class " +
        className + ".\n Maybe your forgot to add the method in the " + className +
        " library wrapper?");
  }

  public static final LocalComparator LOCAL_COMPARATOR = new LocalComparator();

  static class LocalComparator implements Comparator {
    public int compare(Object o, Object o1) {
      Local local1 = (Local) o;
      Local local2 = (Local) o1;
      return local1.getName().compareTo(local2.getName());
    }
  }

  public static final BlockMethodComparator BLOCK_METHOD_COMPARATOR = new BlockMethodComparator();

  static class BlockMethodComparator implements Comparator {
    public int compare(Object o, Object o1) {
      BlockMethod bm1 = (BlockMethod) o;
      BlockMethod bm2 = (BlockMethod) o1;
      return new Integer(bm1.getIndexInMethod()).compareTo(bm2.getIndexInMethod());
    }
  }

  public static final ClassComparator CLASS_COMPARATOR = new ClassComparator();

  static class ClassComparator implements Comparator {
    public int compare(Object o, Object o1) {
      SootClass c1 = (SootClass) o;
      SootClass c2 = (SootClass) o1;
      return c1.getName().compareTo(c2.getName());
    }
  }
}
