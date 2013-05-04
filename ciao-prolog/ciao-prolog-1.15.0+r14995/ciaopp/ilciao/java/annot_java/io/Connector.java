package annot_java.io;

import soot.resources.Resource;
import soot.resources.annotations.Cost;
import soot.resources.annotations.Size;

@Cost(
    resources = {Resource.ANY_RESOURCE},
    functions = {"0"})
public class Connector {

  @Size("[$(0,1),$(0,2),$(0,3)]")
  public native static Object open(String address);
}
