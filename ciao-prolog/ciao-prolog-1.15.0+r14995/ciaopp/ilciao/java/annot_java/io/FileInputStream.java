package annot_java.io;

import soot.resources.Resource;
import soot.resources.annotations.Cost;
import soot.resources.annotations.Size;

@Cost(
    resources = {Resource.ANY_RESOURCE},
    functions = {"0"})
public class FileInputStream {

  @Size("[$(0,1),$(0,2),$(0,3)]")    
  public FileInputStream(String filename) {
  }

  @Cost(
      resources = {Resource.CLOSED_FILES},
      functions = {"1"})
  @Size("[void,$(0,2)]")
  public native void close();

}
