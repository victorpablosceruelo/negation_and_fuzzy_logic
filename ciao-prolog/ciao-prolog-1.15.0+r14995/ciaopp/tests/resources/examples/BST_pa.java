
package examples;

import soot.resources.Resource;
import soot.resources.annotations.Resources;

@Resources({Resource.HEAP_USAGE})
public class BST {
  private List data;
  private BST lc;
  private BST rc;

  public BST() {
    data = new Nil();
    lc = null;
    rc = null;
  }

  /**
   * true
   *   if (types([ret/[examples.BST],this/[examples.BST]]))  {
   *        types([ret/[examples.BST],this/[examples.BST]])
   *   }   * true
   *   if (any([ret,this]))  {
   *        null([ret]) && any([this])
   *   }   * true
   *   if (this/top && ret/top)  {
   *        this/top && ret/top && size(ub,ret,inf) && size(ub,this,size(this))
   *   }
   *  && cost(ub,HEAP_USAGE,8.0*exp(2,size(this))-8.0)
   */
  public BST copy() {
    BST aux = new BST();
    aux.data = data.copy();
    if (lc == null) {
      aux.lc = null;
    } else {
      aux.lc = lc.copy();
    }
    if (rc == null) {
      aux.rc = null;
    } else {
      aux.rc = rc.copy();
    }
    return aux;
  }

  abstract class List {
    abstract List copy();
  }

  class Nil extends List {
  /**
   * true
   *   if (types([ret/[examples.BST$List],this/[examples.BST$Nil]]))  {
   *        types([ret/[examples.BST$List],this/[examples.BST$Nil]])
   *   }   * true
   *   if (any([ret,this]))  {
   *        any([ret,this])
   *   }   * true
   *   if (this/top && ret/top)  {
   *        this/top && ret/top && size(ub,ret,size(this)) && size(ub,this,size(this))
   *   }
   *  && cost(ub,HEAP_USAGE,0)
   */
    List copy() {
      return this;
    }
  }
}



