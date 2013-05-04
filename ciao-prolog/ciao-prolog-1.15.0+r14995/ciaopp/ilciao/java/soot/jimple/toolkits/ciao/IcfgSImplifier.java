package soot.jimple.toolkits.ciao;

import soot.BlockMethod;
import soot.SootClass;
import soot.Unit;
import soot.util.Chain;
import soot.util.SootUtil;

import java.util.*;


public class IcfgSImplifier {

  /**
   * Simplification of the iCFG consist on removals of blockMethods that preserve
   * the semantics. This greatly simplifies latter analyses. To avoid concurrent modifications,
   * first the nodes to delete are identified and the removal happens afterwards.
   *
   * @param blockMethods List of block methods corresponding to the same original method.
   */
  public static void simplifyIcfg(List<BlockMethod> blockMethods) {
    System.out.println("---------------- Before any optimization ----------------");
    SootUtil.printBlockMethodRelationships(blockMethods);
    Collection<BlockMethod> deleted = new ArrayList<BlockMethod>();
    for (BlockMethod blockMethod : blockMethods) {
      if (removeEmptyBlockMethods(blockMethod)) {
        deleted.add(blockMethod);
      }
    }
    blockMethods.removeAll(deleted);
    //System.out.println("---------------- After removal of empty methods --------------");
    //SootUtil.printBlockMethodRelationships(blockMethods);
    for (BlockMethod blockMethod : blockMethods) {
      if (flattenConditionalBlockMethods(blockMethod)) {
        deleted.add(blockMethod);
      }
    }
    blockMethods.removeAll(deleted);
    //System.out.println("---------------- After flattening --------------");
    //SootUtil.printBlockMethodRelationships(blockMethods);
  }

  /**
   * Empty block methods that have no siblings are deleted.
   * Relationships (succs,preds...) are modified accordingly.
   *
   * @param blockMethod A blockMethod in the CFG.
   * @return true if the block method is marked for deletion.
   */
  private static boolean removeEmptyBlockMethods(BlockMethod blockMethod) {
    if (blockMethod.getActiveBody().getUnits().isEmpty()) {
      SortedSet<BlockMethod> succs = blockMethod.getSuccs();
      if (!((blockMethod.isEntry() && succs.isEmpty()) || !blockMethod.getSiblings().isEmpty())) {
        removeBlockMethod(blockMethod);
        System.out.println("[removed empty block method #" + blockMethod.getIndexInMethod() + "]");
        return true;
      }
    }
    return false;
  }

  private static boolean flattenConditionalBlockMethods(BlockMethod blockMethod) {
    if (blockMethod.isConditional()) {
      Set<BlockMethod> preds = blockMethod.getPreds();
//      if (preds.size() > 1) {
//        throw new RuntimeException("Inconsistency detected: the conditional block method " +
//            blockMethod.getBlockMethodName() + " has more than one predecessor: " + preds);
//      }
      Chain units = blockMethod.getActiveBody().getUnits();
      Set<BlockMethod> succs = blockMethod.getSuccs();
      if (units.size() == 1 && succs.size() > 1) {
        Unit guard = (Unit) units.getFirst();
        addGuardToSuccs(blockMethod, guard);
        removeBlockMethod(blockMethod);
        System.out.println(
            "[removed block method #" + blockMethod.getIndexInMethod() + " to flatten iCFG]");
        return true;
      }
    }
    return false;
  }

  private static void addGuardToSuccs(BlockMethod blockMethod, Unit guard) {
    Set<BlockMethod> succs = blockMethod.getSuccs();
    for (BlockMethod succ : succs) {
      if (!succ.isConditional()) {
        throw new RuntimeException("Inconsistency detected: block method " +
            blockMethod.getBlockMethodName() + " has more successor " + succ.getBlockMethodName() +
            ", which is not conditional");
      } else {
        Set<BlockMethod> succPreds = succ.getPreds();
//        if (succPreds.size() > 1) {
//          throw new RuntimeException("Inconsistency detected: the conditional block method " +
//              succ.getBlockMethodName() + " has more than one predecessor: " + succPreds);
//        }
        Chain succUnits = succ.getActiveBody().getUnits();
        succUnits.insertBefore(guard, succUnits.getFirst());
      }
    }
  }

  private static void removeBlockMethod(BlockMethod blockMethod) {
    SootClass classWhereDeclared = blockMethod.getDeclaringClass();
    classWhereDeclared.removeMethod(blockMethod);
    SortedSet<BlockMethod> preds = blockMethod.getPreds();
    SortedSet<BlockMethod> succs = blockMethod.getSuccs();
    SortedSet<BlockMethod> siblings = blockMethod.getSiblings();
    boolean firstChild = true;
    for (BlockMethod pred : preds) {
      pred.getSuccs().remove(blockMethod);
      pred.getSuccs().addAll(succs);
    }
    for (BlockMethod sibling : siblings) {
      sibling.getSiblings().remove(blockMethod);
      sibling.getSiblings().addAll(succs);
    }
    for (BlockMethod succ : succs) {
      classWhereDeclared.removeMethod(succ);
      succ.getPreds().remove(blockMethod);
      succ.getPreds().addAll(preds);
      succ.getSiblings().addAll(siblings);
      succ.setType(blockMethod.getType());
      succ.setFormalParameters(blockMethod.getFormalParameters());
      succ.setParameterTypes(blockMethod.getParameterTypes());
      succ.addAllTagsOf(blockMethod);
      if (blockMethod.isEntry() && firstChild) {
        //calls to the empty, deleted entry block method are redirected to the first child.
        succ.setName(blockMethod.getName());
        firstChild = false;
      }
      classWhereDeclared.addMethod(succ);
    }
  }
}
