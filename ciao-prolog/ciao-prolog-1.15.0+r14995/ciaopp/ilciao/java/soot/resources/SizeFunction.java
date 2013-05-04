package soot.resources;

import soot.BlockMethod;
import soot.resources.annotations.Size;
import soot.util.SootUtil;

import java.util.ArrayList;
import java.util.List;

public class SizeFunction extends GenericResourceFunction {

  SizeFunction() {
    super(Resource.SIZE);
  }

  @Override
  public String getCost(BlockMethod blockMethod) {
    Size sizeAnnotation = (Size) SootUtil.getAnnotation(blockMethod, Size.class);
    if (sizeAnnotation != null) {
      return sizeAnnotation.value();
    } else {
      return getDefaultBlockMethodCost(blockMethod);
    }
  }

  @Override
  String getDefaultBlockMethodCost(BlockMethod blockMethod) {
    List<String> result = new ArrayList<String>();
    for (int i = 0; i < blockMethod.getFormalParameterCount(); i++) {
      result.add(INFINITY);
    }
    return result.toString();
  }

}
