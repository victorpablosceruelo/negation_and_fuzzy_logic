package annot_java.lang;

import soot.resources.Resource;
import soot.resources.annotations.Cost;
import soot.resources.annotations.Size;

@Cost(
    resources = {Resource.ANY_RESOURCE, Resource.ENERGY_CONSUMED},
    functions = {"0", "50"})
public class System {

  @Size("[$(0,1),$(0,2)]")
  public native static int currentTimeMillis();

}

// NOTE about "EnergyConsumed" resource: Since we cannot analyze the code
// for this library, we will assume a constant energy consumption of 50
// microjoules for currentTimeMillis().
