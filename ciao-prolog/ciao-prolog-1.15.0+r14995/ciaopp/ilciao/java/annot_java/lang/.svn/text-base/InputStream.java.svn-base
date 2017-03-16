package annot_java.lang;

import soot.resources.Resource;
import soot.resources.annotations.Cost;
import soot.resources.annotations.Size;

@Cost(
    resources = {Resource.ANY_RESOURCE},
    functions = {"0"})
public class InputStream {
  public native void close();

  @Size("[-($(0,2),1),$(0,2)]")
  public native InputStream next();

  @Cost(
      resources = {Resource.BYTES_RECEIVED, Resource.SCREEN_WIDTH},
      functions = {"2", "1"})
  @Size("[1,$(0,2)]")
  public native char read();

}

