package annot_java.net;

import soot.resources.Resource;
import soot.resources.annotations.Cost;
import soot.resources.annotations.Size;


@Cost(
    resources = {Resource.ANY_RESOURCE},
    functions = {"0"})
public class URLEncoder {

  @Size("[*(6,$(0,3)), void, $(0,3)]")
  public native String encode(String s);

}
