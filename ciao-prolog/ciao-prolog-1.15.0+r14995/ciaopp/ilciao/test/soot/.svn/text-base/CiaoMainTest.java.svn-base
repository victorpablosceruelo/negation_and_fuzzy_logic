package soot;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import soot.jimple.toolkits.ciao.ClauseGenerator;
import soot.options.CiaoOptions;
import soot.options.Options;
import util.StreamComparator;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;

/**
 * CiaoMain Tester.
 *
 * @author Mario Mendez-Lojo
 * @since <pre>02/11/2008</pre>
 */
public class CiaoMainTest extends TestCase {

  public CiaoMainTest(String name) {
    super(name);
  }

  public void testMain() throws FileNotFoundException {
    String[] testClasses = {"examples.Dhrystone"};
    InputStream outputStream, expected;
    for (String testClass : testClasses) {
      CiaoOptions.v().setMainClass(testClass);
      CiaoOptions.v().setVerboseIlCiao(false);
      CiaoMain.main();
      String outputFilePath = Options.v().output_dir() + "/" + ClauseGenerator.getOutputFileName();
      String expectedFilePath =
          CiaoOptions.v().getBaseDir() + "/test/soot/" + testClass + ".pl.expected";
      outputStream = new FileInputStream(outputFilePath);
      expected = new FileInputStream(expectedFilePath);
      StreamComparator.assertEquals(outputStream, expected, true);
      CiaoMain.reset();
    }
  }

  public static Test suite() {
    return new TestSuite(CiaoMainTest.class);
  }
}
