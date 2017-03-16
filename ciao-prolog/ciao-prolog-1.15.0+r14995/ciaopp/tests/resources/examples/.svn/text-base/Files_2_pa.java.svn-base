
package examples;

import annot_java.io.*;
import soot.resources.Resource;
import soot.resources.annotations.Resources;

@Resources({Resource.DATA_STORED, Resource.CLOSED_FILES, Resource.OPENED_FILES})
public class Files_2 {

  /**
   * true
   *   if (types([ret/[void],this/[examples.Files_2]]))  {
   *        types([ret/[void],this/[examples.Files_2]])
   *   }   * true
   *   if (null([ret]) && any([this]))  {
   *        null([ret]) && any([this])
   *   }   * true
   *   if (this/top && ret/top)  {
   *        this/top && ret/top && size(ub,ret,1) && size(ub,this,size(this))
   *   }
   *  && cost(ub,CLOSED_FILES,2*5$1+2),cost(ub,OPENED_FILES,2*5$1+2),cost(ub,DATA_STORED,5$1*4$1+5$1*2$1+4$1+2$1)
   */
  public void CopyFiles() {
    java.lang.String dirname = "./website";
    File f1 = new File(dirname);
    java.lang.String files[] = f1.list();
    int len = f1.length();
    copyEachFile(len, files);
  }

  /**
   * true
   *   if (types([ret/[void],this/[examples.Files_2],arg(1)/[int],arg(2)/[java.lang.String]]))  {
   *        types([ret/[void],this/[examples.Files_2],arg(1)/[int],arg(2)/[java.lang.String]])
   *   }   * true
   *   if (null([ret,arg(1)]) && any([this,arg(2)]))  {
   *        null([ret,arg(1)]) && any([this,arg(2)])
   *   }   * true
   *   if (arg(2)/top && arg(1)/top && this/top && ret/top)  {
   *        arg(2)/top && arg(1)/top && this/top && ret/top && size(ub,ret,1) && size(ub,this,size(this)) && size(ub,arg(1),int(arg(1))) && size(ub,arg(2),size(arg(2)))
   *   }
   *  && cost(ub,CLOSED_FILES,2*int(arg(1))+2),cost(ub,OPENED_FILES,2*int(arg(1))+2),cost(ub,DATA_STORED,4$1*int(arg(1))+4$1+2$1*int(arg(1))+2$1)
   */
  void copyEachFile(int i, java.lang.String files[]) {
    if (i != 0) {
      annot_java.lang.StringBuffer content = readFile(files[i]);
      java.lang.String out = files[i] + "_out";
      writeFile(out, content);
      i = i - 1;
      copyEachFile(i, files);
    }
  }

  /**
   * true
   *   if (types([ret/[annot_java.lang.StringBuffer],this/[examples.Files_2],arg(1)/[java.lang.String]]))  {
   *        types([ret/[annot_java.lang.StringBuffer],this/[examples.Files_2],arg(1)/[java.lang.String]])
   *   }   * true
   *   if (any([ret,this,arg(1)]))  {
   *        any([this,arg(1)])
   *   }   * true
   *   if (arg(1)/top && this/top && ret/top)  {
   *        arg(1)/top && this/top && ret/top && size(ub,ret,4$1+2$1) && size(ub,this,size(this)) && size(ub,arg(1),size(arg(1)))
   *   }
   *  && cost(ub,CLOSED_FILES,1),cost(ub,OPENED_FILES,1),cost(ub,DATA_STORED,0)
   */
  public annot_java.lang.StringBuffer readFile(java.lang.String filename) {
    FileInputStream fstream = new FileInputStream(filename);
    DataInputStream in = new DataInputStream(fstream);

    annot_java.lang.StringBuffer fcontent = read(in);
    fstream.close();
    return fcontent;
  }

  /**
   * true
   *   if (types([ret/[annot_java.lang.StringBuffer],this/[examples.Files_2],arg(1)/[annot_java.io.DataInputStream]]))  {
   *        types([ret/[annot_java.lang.StringBuffer],this/[examples.Files_2],arg(1)/[annot_java.io.DataInputStream]])
   *   }   * true
   *   if (any([ret,this,arg(1)]))  {
   *        any([ret,this,arg(1)])
   *   }   * true
   *   if (arg(1)/top && this/top && ret/top)  {
   *        arg(1)/top && this/top && ret/top && size(ub,ret,2$1+size(arg(1))) && size(ub,this,size(this)) && size(ub,arg(1),size(arg(1)))
   *   }
   *  && cost(ub,CLOSED_FILES,0),cost(ub,OPENED_FILES,0),cost(ub,DATA_STORED,0)
   */
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
  /**
   * true
   *   if (types([ret/[void],this/[examples.Files_2],arg(1)/[java.lang.String],arg(2)/[annot_java.lang.StringBuffer]]))  {
   *        types([ret/[void],this/[examples.Files_2],arg(1)/[java.lang.String],arg(2)/[annot_java.lang.StringBuffer]])
   *   }   * true
   *   if (null([ret]) && any([this,arg(1),arg(2)]))  {
   *        null([ret]) && any([this,arg(1),arg(2)])
   *   }   * true
   *   if (arg(2)/top && arg(1)/top && this/top && ret/top)  {
   *        arg(2)/top && arg(1)/top && this/top && ret/top && size(ub,ret,1) && size(ub,this,size(this)) && size(ub,arg(1),size(arg(1))) && size(ub,arg(2),size(arg(2)))
   *   }
   *  && cost(ub,CLOSED_FILES,1),cost(ub,OPENED_FILES,1),cost(ub,DATA_STORED,size(arg(2)))
   */
    DataOutputStream p;
    out = new FileOutputStream("myfile.txt");
    p = new DataOutputStream(out);

    p.writeChars(buf.toString());
    out.close();
  }
}


