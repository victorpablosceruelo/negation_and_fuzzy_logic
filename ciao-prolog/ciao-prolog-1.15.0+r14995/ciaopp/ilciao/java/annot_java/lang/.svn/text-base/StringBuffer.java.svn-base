package annot_java.lang;

import soot.resources.Resource;
import soot.resources.annotations.Cost;
import soot.resources.annotations.Size;

@Cost(
    resources = {Resource.ANY_RESOURCE},
    functions = {"0"})
public class StringBuffer {

  @Size("[$(0,1),$(0,2)]")
  public StringBuffer() {
  }

  // 30 is just an arbitrarily amount.  
  @Cost(
    resources = {Resource.ENERGY_CONSUMED},
    functions = {"30"})
  @Size("[+(1,$(0,2)),$(0,2),1]")
  public native StringBuffer append(char c);

  @Cost(
    resources = {Resource.ENERGY_CONSUMED},
    functions = {"*(30,$(0,3))"})
  @Size("[+($(0,3),$(0,2)),$(0,2),$(0,3)]")
  public native StringBuffer append(java.lang.String s);

  @Size("[$(0,2),$(0,2)]")
  public native java.lang.String toString();

  @Size("[1,$(0,2)]")
  public native char read();
}
