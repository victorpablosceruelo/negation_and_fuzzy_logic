package annot_java.io;

import soot.resources.Resource;
import soot.resources.annotations.Cost;
import soot.resources.annotations.Size;

@Cost(
    resources = {Resource.ANY_RESOURCE},
    functions = {"0"})
public class File {

  @Size("[void,$(0,2),$(0,3)]")
  public File(String dirname) {
  }

  @Size("[$(0,1),$(0,2)]")
  public native String[] list();

  @Size("[$(0,1),$(0,2)]")
  public native int length();
}
