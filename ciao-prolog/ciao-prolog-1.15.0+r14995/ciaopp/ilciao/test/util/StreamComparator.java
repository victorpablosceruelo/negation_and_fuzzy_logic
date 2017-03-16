package util;

import junit.framework.AssertionFailedError;

import java.io.IOException;
import java.io.InputStream;


public class StreamComparator {

  public static void assertEquals(InputStream is1, InputStream is2, boolean ignoreWhitespace) {
    assertEquals("Files are not equal", is1, is2, ignoreWhitespace);
  }

  public static void assertEquals(String message, InputStream is1, InputStream is2,
      boolean ignoreWhitespace) throws AssertionFailedError {
    if (is1 == is2) {
      return;
    }
    if (is1 == null || is2 == null) {
      throw new AssertionFailedError(message);
    }
    try {
      while (true) {
        int c1 = is1.read();
        while (ignoreWhitespace && isWhitespace(c1)) {
          c1 = is1.read();
        }
        int c2 = is2.read();
        while (ignoreWhitespace && isWhitespace(c2)) {
          c2 = is2.read();
        }
        if (c1 == -1 && c2 == -1) {
          return;
        }
        if (c1 != c2) {
          throw new AssertionFailedError(message);
        }
      }
    } catch (IOException e) {
      throw new RuntimeException("I/O error when comparing files.");
    }
  }

  private static boolean isWhitespace(int c) {
    return c != -1 && Character.isWhitespace((char) c);
  }
}
