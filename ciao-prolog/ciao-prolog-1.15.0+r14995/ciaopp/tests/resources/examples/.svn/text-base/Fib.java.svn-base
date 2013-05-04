package examples;

import soot.resources.Resource;
import soot.resources.annotations.Resources;

@Resources({Resource.STEPS})
public class Fib {

  public int fib(int n) {
    if (n == 0) {
      return 0;
    } else if (n == 1) {
      return 1;
    } else {
      return fib(n - 1) + fib(n - 2);
    }
  }

//  public int fib2(int n) {
//    if (n <= 1) {
//      return 1;
//    } else {
//      return (fib2(n - 1) + fib2(n - 2));
//    }
//  }
}



