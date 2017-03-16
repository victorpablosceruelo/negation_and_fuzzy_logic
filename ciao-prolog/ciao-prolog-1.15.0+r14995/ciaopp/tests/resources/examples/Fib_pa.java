
package examples;

import soot.resources.Resource;
import soot.resources.annotations.Resources;

@Resources({Resource.STEPS})
public class Fib {

  /**
   * true
   *   if (types([ret/[int],this/[examples.Fib],arg(1)/[int]]))  {
   *        types([ret/[int],this/[examples.Fib],arg(1)/[int]])
   *   }   * true
   *   if (null([ret,arg(1)]) && any([this]))  {
   *        null([ret,arg(1)]) && any([this])
   *   }   * true
   *   if (arg(1)/top && this/top && ret/top)  {
   *        arg(1)/top && this/top && ret/top && size(ub,ret,0.4472135954999579*exp(1.618033988749895,int(arg(1)))-0.4472135954999579*exp(-0.6180339887498949,int(arg(1)))) && size(ub,this,size(this)) && size(ub,arg(1),int(arg(1)))
   *   }
   *  && cost(ub,STEPS,7.683281572999747*exp(1.618033988749895,int(arg(1)))+2.316718427000253*exp(-0.6180339887498949,int(arg(1)))-8.0)
   */
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



