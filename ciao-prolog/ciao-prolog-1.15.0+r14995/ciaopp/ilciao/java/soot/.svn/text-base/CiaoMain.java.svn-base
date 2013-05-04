package soot;

import soot.jimple.toolkits.ciao.ClauseGenerator;
import soot.jimple.toolkits.ciao.IcfgGenerator;
import soot.options.CiaoOptions;

public class CiaoMain {

  public static void main(String[] args) {
    CiaoOptions.v().loadArgs(args);
    CiaoMain.main();
  }

  public static void generateCiao(String baseDir, String mainClass) {
    CiaoOptions.v().setMainClass(mainClass);
    CiaoOptions.v().setBaseDir(baseDir);
    CiaoOptions.v().setVerboseIlCiao(false);
    CiaoMain.main();
  }

  public static void main() {
    Pack pack = PackManager.v().getPack("jtp");
    pack.add(new Transform("jtp.mg", IcfgGenerator.v()));
    String[] sootArgs = {CiaoOptions.v().getMainClass(), "lang.Builtin"};
    Main.main(sootArgs);
    new ClauseGenerator().printCiao();
  }

  public static void reset() {
    G.reset();
  }
}
