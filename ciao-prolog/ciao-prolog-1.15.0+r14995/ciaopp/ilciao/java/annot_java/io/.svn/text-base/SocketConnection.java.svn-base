package annot_java.io;

import annot_java.lang.InputStream;
import soot.resources.Resource;
import soot.resources.annotations.Cost;
import soot.resources.annotations.Size;

@Cost(
    resources = {Resource.ANY_RESOURCE},
    functions = {"0"})
public class SocketConnection {

  @Size("[$(0,1),$(0,2)]")
  public native InputStream openInputStream();
  @Size("[void,$(0,2)]")
  public native void close();
}
