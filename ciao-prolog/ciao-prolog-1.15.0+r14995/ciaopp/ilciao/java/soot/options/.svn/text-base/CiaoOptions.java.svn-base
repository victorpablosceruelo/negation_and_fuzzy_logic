package soot.options;

import soot.util.SootUtil;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class CiaoOptions extends OptionsBase {

  private static CiaoOptions instance;
  private boolean preservingOriginalNames;

  // Boolean options, in alphabetical order
  private boolean project;
  public boolean resourcesMode;
  private boolean verboseIlCiao;

  // Non-boolean, one possible value options
  private String baseDir;
  private String mainClass;

  // Multiple value options
  private List<String> includeFiles;

  private CiaoOptions() {
    setProject(false);
    setResourcesMode(true);
    setVerboseIlCiao(true);
    includeFiles = new ArrayList<String>();
    setBaseDir(System.getProperty("user.dir"));//dir from which java is invoked
    setSootOptions();
  }

  public static CiaoOptions v() {
    if (instance == null) {
      instance = new CiaoOptions();
    }
    return instance;
  }

  public void loadArgs(String[] args) {
    int i = 0;
    while (i < args.length) {
      String argi = args[i];
      if (argi.equals("-project")) {
        setProject(getBoolFromOption(args[i + 1]));
      } else if (argi.equals("-resources-mode")) {
        setResourcesMode(getBoolFromOption(args[i + 1]));
      } else if (argi.equals("-verbose-ilciao")) {
        setVerboseIlCiao(getBoolFromOption(args[i + 1]));
      } else if (argi.equals("-base-dir")) {
        setBaseDir(args[i + 1]);
      } else if (argi.equals("-main-class")) {
        setMainClass(args[i + 1]);
      } else if (argi.equals("-include-files")) {
        setIncludeFiles(readCsvValues(args[i + 1]));
      } else {
        throw new RuntimeException("Invalid option: " + argi);
      }
      i += 2;
    }
    setSootOptions();
  }

  private boolean getBoolFromOption(String arg) {
    if (arg.toLowerCase().equals("on")) {
      return true;
    } else if (arg.toLowerCase().equals("off")) {
      return false;
    } else {
      throw new RuntimeException("Invalid flag value: " + arg + ". Only [on|off] is permitted.");
    }
  }

  private List<String> readCsvValues(String csvValue) {
    if (csvValue != null) {
      return SootUtil.getListFromCsv(csvValue);
    } else {
      return new ArrayList<String>();
    }
  }

  private void transformRelativeIntoAbsolutePaths() {
    for (int i = 0; i < includeFiles.size(); i++) {
      String includeFile = includeFiles.get(i);
      includeFiles.set(i, baseDir + "/" + includeFile);
    }
  }

  /**
   * Set Soot Options necessary for correct behavior of the Ciao phase.
   * Invoked after setting all the local (IlCiao) options.
   */
  private void setSootOptions() {
    //Options.v().set_verbose(true);
    Options.v().set_keep_line_number(true); //preserve line numbers
    Options.v().setPhaseOption("jop", "on"); //optimizations that might simplify the CFG
    // The next three options are equivalent to the command-line option "-trim-cfgs"
    // , which is not directly available on the Options API.
    // Their purpose is to minimize the number of edges in the CFG
    // By only including exceptional edges from those units which can
    // raise an exception type compatible with the one caught.
    Options.v().set_throw_analysis(Options.throw_analysis_unit);
    Options.v().set_omit_excepting_unit_edges(true);
    Options.v().setPhaseOption("jb.tt", "on");
    Options.v().set_app(true); //application mode
    Options.v().set_output_format(Options.output_format_none);
    String[] exclude =
        {"soot.resources", "soot.resources.annotations"}; //do not include annotations in iCFG
    Options.v().set_exclude(Arrays.asList(exclude));
    if (!isResourcesMode()) {
      // keep variable names. A Soot bug causes this option to
      // give the same name to all SSA versions, thus we do not
      // preserve original names in that case.
      Options.v().setPhaseOption("jb", "use-original-names:true"); // preserve variable names
      preservingOriginalNames = true;
    }
  }

  public boolean isVerboseIlCiao() {
    return verboseIlCiao;
  }

  public boolean isResourcesMode() {
    return resourcesMode;
  }

  public void setResourcesMode(boolean rm) {
    resourcesMode = rm;
    if (rm) {
      setProject(true);
    }
    setSootOptions();
  }

  public String getMainClass() {
    return mainClass;
  }

  public void setMainClass(String mainClass) {
    this.mainClass = mainClass;
  }

  public String getBaseDir() {
    return baseDir;
  }

  public List<String> getIncludeFiles() {
    return includeFiles;
  }

  public void setBaseDir(String baseDir) {
    this.baseDir = baseDir;
    Options.v().set_output_dir(baseDir);
    transformRelativeIntoAbsolutePaths();
  }

  public void setVerboseIlCiao(boolean verboseIlCiao) {
    this.verboseIlCiao = verboseIlCiao;
  }

  public void setIncludeFiles(List<String> includeFiles) {
    this.includeFiles = includeFiles;
    transformRelativeIntoAbsolutePaths();
  }

  public boolean isProject() {
    return project;
  }

  public void setProject(boolean project) {
    this.project = project;
  }

  public boolean isPreservingOriginalNames() {
    return preservingOriginalNames;
  }
}
