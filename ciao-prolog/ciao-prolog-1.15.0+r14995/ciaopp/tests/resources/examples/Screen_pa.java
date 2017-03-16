
// MIDP (Mobile Information Device Profile) provides GUI, network
// implementation, and local data storage on top of the CLDC (Connected
// Limited Device Configuration) configuration, targeting devices such as
// mobile phones or PDAs. 

// In particular, this example uses a Java API for Bluetooth Wireless
// Technology and connects to a newly found device and after successful
// connection, one character after the other is read from the stream, until
// a newline character is encountered. After the connections are closed,
// the line we read is returned from the method. 

// Assuming that some of the requirements stated by MIDP is the screen size
// (e.g., 96*54 pixels), we want to estimate the screen width at compile
// time.


package examples;

//import java.io.IOException;
//import java.io.InputStream;

import annot_java.io.Connector;
import annot_java.io.StreamConnection;
import annot_java.lang.InputStream;
import static soot.resources.Resource.SCREEN_WIDTH;
import static soot.resources.Resource.STEPS;
import soot.resources.annotations.Resources;

@Resources({STEPS, SCREEN_WIDTH})

public class Screen {
  private String btAddress;

  public String connectAndReadLine() {
  /**
   * true
   *   if (types([ret/[java.lang.String],this/[examples.Screen]]))  {
   *        types([ret/[java.lang.String],this/[examples.Screen]])
   *   }   * true
   *   if (any([ret,this]))  {
   *        any([ret,this])
   *   }   * true
   *   if (this/top && ret/top)  {
   *        this/top && ret/top && size(ub,ret,11$1+2$1) && size(ub,this,size(this))
   *   }
   *  && cost(ub,STEPS,6*11$1+22),cost(ub,SCREEN_WIDTH,11$1)
   */

    if (btAddress == null) {
      return null;
    }

    StreamConnection conn = (StreamConnection) Connector.open("btspp://" + btAddress + ":1");
    InputStream is = conn.openInputStream();
    StringBuffer buffer = readLine(is);

    is.close();
    conn.close();

    return buffer.toString();
  }

  /**
   * true
   *   if (types([ret/[java.lang.StringBuffer],this/[examples.Screen],arg(1)/[annot_java.lang.InputStream]]))  {
   *        types([ret/[java.lang.StringBuffer],this/[examples.Screen],arg(1)/[annot_java.lang.InputStream]])
   *   }   * true
   *   if (any([ret,this,arg(1)]))  {
   *        any([ret,this,arg(1)])
   *   }   * true
   *   if (arg(1)/top && this/top && ret/top)  {
   *        arg(1)/top && this/top && ret/top && size(ub,ret,2$1+size(arg(1))) && size(ub,this,size(this)) && size(ub,arg(1),size(arg(1)))
   *   }
   *  && cost(ub,STEPS,6*size(arg(1))+4),cost(ub,SCREEN_WIDTH,size(arg(1)))
   */
  StringBuffer readLine(InputStream s) {
    if (s == null) {
      return new StringBuffer();
    } else {
      char c = s.read();
      StringBuffer buffer = readLine(s.next());
      return buffer.append(c);
    }
  }
}







