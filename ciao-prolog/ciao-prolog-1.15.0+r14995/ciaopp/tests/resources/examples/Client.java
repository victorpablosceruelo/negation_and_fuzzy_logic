// MIDP (Mobile Information Device Profile) provides GUI, network
// implementation, and local data storage on top of the CLDC (Connected
// Limited Device Configuration) configuration, targeting devices such as
// mobile phones or PDAs. 

// In particular, this example establishes a socket connection with a time
// server running on port 13 on a remote machine. The client doesn't send
// any data; the server treats the new connection itself as a service
// request, and immediately responds with the current date and time.
// http://developers.sun.com/mobility/midp/articles/midp2network/
// Copyright 1994-2008 Sun Microsystems, Inc. 

package examples;

//import java.io.IOException;
//import java.io.InputStream;

import annot_java.io.Connector;
import annot_java.io.SocketConnection;
import annot_java.lang.InputStream;
import static soot.resources.Resource.BYTES_RECEIVED;
import static soot.resources.Resource.STEPS;
import soot.resources.annotations.Resources;


@Resources({STEPS, BYTES_RECEIVED})

public class Client {
  private java.lang.String remoteTimeServerAddress;

  public void timeClient() {
    //if (remoteTimeServerAddress != null) {
    SocketConnection conn =
        (SocketConnection) Connector.open("socket://" + remoteTimeServerAddress + ":13");
    InputStream s = conn.openInputStream();
    annot_java.lang.StringBuffer buffer = readFromServer(s);
    //System.out.println("Time is " + buffer.toString());

    s.close();
    conn.close();
    //}
  }

  annot_java.lang.StringBuffer readFromServer(InputStream s) {
    if (s == null) {
      return new annot_java.lang.StringBuffer();
    } else {
      char c = s.read();
      annot_java.lang.StringBuffer buffer = readFromServer(s.next());
      return buffer.append(c);
    }
  }
}






