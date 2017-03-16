package annot_java.io;

import soot.resources.Resource;
import soot.resources.annotations.Cost;
import soot.resources.annotations.Size;

@Cost(
    resources = {Resource.ANY_RESOURCE},
    functions = {"0"})
public class DataInputStream {
  @Size("[$(0,1), $(0,2), $(0,3)]")
  public DataInputStream(FileInputStream stream) {}
  @Size("[-($(0,2),1),$(0,2)]")
  public native DataInputStream next();
  @Size("[1,$(0,2)]")
  public native char readChar();
}
