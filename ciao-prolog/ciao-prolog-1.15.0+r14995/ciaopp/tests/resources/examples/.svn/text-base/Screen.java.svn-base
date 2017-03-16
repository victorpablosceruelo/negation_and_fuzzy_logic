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







