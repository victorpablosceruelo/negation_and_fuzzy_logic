package annot_java.lang;

import soot.resources.Resource;
import soot.resources.annotations.Cost;
import soot.resources.annotations.Size;

@Cost(
    resources = {Resource.ANY_RESOURCE},
    functions = {"0"})

public class StringBuilder {

  @Size("[$(0,1),$(0,2)]")
  public StringBuilder() {
  }

  @Size("[+(1,$(0,2)),$(0,2),1]")
  public native StringBuilder append(char c);

  @Size("[+($(0,2),$(0,3)),$(0,2),$(0,3)]")
  public native StringBuilder append(String s);

  @Size("[$(0,2),$(0,2)]")
  public native java.lang.String toString();

}
