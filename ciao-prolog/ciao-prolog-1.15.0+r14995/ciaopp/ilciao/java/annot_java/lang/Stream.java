package annot_java.lang;

import soot.resources.Resource;
import soot.resources.annotations.Cost;
import soot.resources.annotations.Measure;

@Cost(
    resources = {Resource.ANY_RESOURCE},
    functions = {"0"})
public class Stream {

  @Measure(retMeasure = Measure.MeasureType.IGNORE)
  Stream() {
  }

  @Cost(
      resources = {Resource.COST_IN_DOLLARS},
      functions = {"*(2,$(0,3))"})
  public native void send(java.lang.String data);
}
