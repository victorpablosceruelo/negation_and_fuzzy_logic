package examples;

import soot.resources.Resource;
import soot.resources.annotations.Resources;

@Resources({Resource.STEPS})
public class Fact {

  public int fact(int n) {
    if (n == 0) return 1;
    else return n * fact(n - 1);
  }

//   public int fact2(int n) {
//     if (n == 0) {
//       return 1;
//     } else if (n == 1) {
//       return 1;
//     } else {
//       return n * fact2(n - 1);
//     }
//   }
}

// public class examples.Fact extends java.lang.Object{
// public examples.Fact();
//   Code:
//    0:	aload_0
//    1:	invokespecial	#1; //Method java/lang/Object."<init>":()V
//    4:	return

// public int fact(int);
//   Code:
//    0:	iload_1
//    1:	ifne	6
//    4:	iconst_1
//    5:	ireturn
//    6:	iload_1
//    7:	aload_0
//    8:	iload_1
//    9:	iconst_1
//    10:	isub
//    11:	invokevirtual	#2; //Method fact:(I)I
//    14:	imul
//    15:	ireturn

// }
