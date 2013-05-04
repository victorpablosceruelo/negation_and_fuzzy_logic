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
    List copy() {
      return this;
    }
  }
}



