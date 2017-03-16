package annot_java.lang;

import soot.resources.Resource;
import soot.resources.annotations.Cost;
import soot.resources.annotations.Size;

@Cost(
    resources = {Resource.ANY_RESOURCE},
    functions = {"0"})
public class String {

  @Size("[1,$(0,2),$(0,3)]")
  public native char charAt(int index);

  @Size("[$(0,1),$(0,2),$(0,3)]")
  public native int compareTo(java.lang.String string);

  @Size("[$(0,2), $(0,2)]")
  public native java.lang.String trim();

}
