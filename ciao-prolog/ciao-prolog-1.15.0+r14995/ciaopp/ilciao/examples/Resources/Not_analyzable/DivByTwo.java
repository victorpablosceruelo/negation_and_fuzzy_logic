package examples;

import soot.resources.Resource;
import soot.resources.annotations.Resources;

@Resources({Resource.STEPS})
public class DivByTwo {

  static int divByTwo(int n) {
    int acu = 0;
    while (n > 0) {
      n = n / 2;
      acu++;
    }
    return acu;
  }

// public class examples.DivByTwo extends java.lang.Object{
// public examples.DivByTwo();
//   Code:
//    0:	aload_0
//    1:	invokespecial	#1; //Method java/lang/Object."<init>":()V
//    4:	return

// static int divByTwo(int);
//   Code:
//    0:	iconst_0
//    1:	istore_1
//    2:	iload_0
//    3:	ifle	16
//    6:	iload_0
//    7:	iconst_2
//    8:	idiv
//    9:	istore_0
//    10:	iinc	1, 1
//    13:	goto	2
//    16:	iload_1
//    17:	ireturn

// }

//  static int divByTwoSsaFriendly(int n) {
//    if (n > 1) {
//      return 1 + divByTwoSsaFriendly(n / 2);
//    } else {
//      return 0;
//    }
//  }
}
