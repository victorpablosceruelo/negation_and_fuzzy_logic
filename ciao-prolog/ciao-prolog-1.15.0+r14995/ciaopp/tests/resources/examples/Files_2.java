package examples;

import annot_java.io.*;
import soot.resources.Resource;
import soot.resources.annotations.Resources;

@Resources({Resource.DATA_STORED, Resource.CLOSED_FILES, Resource.OPENED_FILES})
public class Files_2 {

  public void CopyFiles() {
    java.lang.String dirname = "./website";
    File f1 = new File(dirname);
    java.lang.String files[] = f1.list();
    int len = f1.length();
    copyEachFile(len, files);
  }

  void copyEachFile(int i, java.lang.String files[]) {
    if (i != 0) {
      annot_java.lang.StringBuffer content = readFile(files[i]);
      java.lang.String out = files[i] + "_out";
      writeFile(out, content);
      i = i - 1;
      copyEachFile(i, files);
    }
  }

  public annot_java.lang.StringBuffer readFile(java.lang.String filename) {
    FileInputStream fstream = new FileInputStream(filename);
    DataInputStream in = new DataInputStream(fstream);

    annot_java.lang.StringBuffer fcontent = read(in);
    fstream.close();
    return fcontent;
  }

  annot_java.lang.StringBuffer read(DataInputStream in) {
    if (in == null) return new annot_java.lang.StringBuffer();
    else {
      char c = in.readChar();
      annot_java.lang.StringBuffer buffer = read(in.next());
      return buffer.append(c);
    }
  }

  public void writeFile(java.lang.String filename, annot_java.lang.StringBuffer buf) {
    FileOutputStream out;
    DataOutputStream p;
    out = new FileOutputStream("myfile.txt");
    p = new DataOutputStream(out);

    p.writeChars(buf.toString());
    out.close();
  }
}


