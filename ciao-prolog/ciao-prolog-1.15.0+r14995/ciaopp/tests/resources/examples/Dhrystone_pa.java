
package examples;

import soot.resources.Resource;
import soot.resources.annotations.Resources;

@Resources({Resource.ENERGY_CONSUMED})
public class Dhrystone {
  //implements Runnable {

  // GlobalVariables
//  static Record_Type Record_Glob, Next_Record_Glob;
//  static int Int_Glob;
//  static boolean Bool_Glob;
//  static char Char_Glob_1, Char_Glob_2;
//  static Record_Type First_Record = new Record_Type(), Second_Record = new Record_Type();


  /**
   * true
   *   if (types([ret/[void],this/[examples.Dhrystone]]))  {
   *        types([ret/[void],this/[examples.Dhrystone]])
   *   }   * true
   *   if (null([ret]) && any([this]))  {
   *        null([ret]) && any([this])
   *   }   * true
   *   if (this/top && ret/top)  {
   *        this/top && ret/top && size(ub,ret,1) && size(ub,this,size(this))
   *   }
   *  && cost(ub,ENERGY_CONSUMED,496605216)
   */
  public void execute() {
    int Number_Of_Runs = 100000;
    String String_Loc_1;


    long begin_time,
        end_time,
        total_time;

//    Next_Record_Glob = Second_Record;
//    Record_Glob = First_Record;
//
//    Record_Glob.Record_Comp = Next_Record_Glob;
//    Record_Glob.Discr = 1;
//    Record_Glob.Enum_Comp = 3;
//    Record_Glob.Int_Comp = 40;
//    Record_Glob.String_Comp = "DHRYSTONE PROGRAM, SOME STRING";

    String_Loc_1 = "DHRYSTONE PROGRAM, 1'ST STRING";

    //Msg.out.println("Execution starts, " + Number_Of_Runs + " runs through Dhrystone");

    begin_time = System.currentTimeMillis();
    executeRuns(Number_Of_Runs);
//     Run_Index = 1;
//     while (Run_Index <= Number_Of_Runs) {

//     }

    end_time = System.currentTimeMillis();
    total_time = end_time - begin_time;

    //Msg.out.println("total time: " + total_time + "ms");
    //Msg.out.println("Result: " + Number_Of_Runs * 1000 / total_time + " dhrystone/sec.");

  }

  void executeRuns(int Number_Of_Runs) {

    String String_Loc_2;
    int Enum_Loc_0;
    int Int_Loc_1, Int_Loc_2, Int_Loc_3;
    int Int_Loc_3_Ref_0;
  /**
   * true
   *   if (types([ret/[void],this/[examples.Dhrystone],arg(1)/[int]]))  {
   *        types([ret/[void],this/[examples.Dhrystone],arg(1)/[int]])
   *   }   * true
   *   if (null([ret,arg(1)]) && any([this]))  {
   *        null([ret,arg(1)]) && any([this])
   *   }   * true
   *   if (arg(1)/top && this/top && ret/top)  {
   *        arg(1)/top && this/top && ret/top && size(ub,ret,1) && size(ub,this,size(this)) && size(ub,arg(1),int(arg(1)))
   *   }
   *  && cost(ub,ENERGY_CONSUMED,4966*int(arg(1))+4966)
   */
    int Int_Loc_1_Ref_0;
    Record_Type Record_Glob = null;
    int[] Array_Glob_1 = new int[128];
    int[] Array_Glob_2 = new int[128];
    int Run_Index;
    //	ExitObserver exitObserver;

    if (Number_Of_Runs > 0) {
      Proc_5();
      Proc_4();
      String_Loc_2 = "DHRYSTONE PROGRAM, 2'ND STRING";

      Enum_Loc_0 = 2;
      //      Bool_Glob = !Func_2(String_Loc_1, String_Loc_2);

      Int_Loc_1 = 2;
      Int_Loc_2 = 3;

      Int_Loc_3_Ref_0 = 5 * Int_Loc_1 - Int_Loc_2;
      Proc_7(Int_Loc_1, Int_Loc_2, Int_Loc_3_Ref_0);
      Int_Loc_3_Ref_0 = 5 * (Int_Loc_1 + 1) - Int_Loc_2;
      Proc_7((Int_Loc_1 + 1), Int_Loc_2, Int_Loc_3_Ref_0);


      Int_Loc_3 = Int_Loc_3_Ref_0;
      Proc_8(Array_Glob_1, Array_Glob_2, Int_Loc_1, Int_Loc_3);
      Proc_1(Record_Glob);

      // We could remove this loop
      //      for (Char_Index = 'A'; Char_Index <= Char_Glob_2; ++Char_Index) {
      //        if (Enum_Loc_0 == Func_1(Char_Index, 'C')) {
      //          Proc_6(1, Enum_Loc);
      //        }
      //      }

      Int_Loc_3 = Int_Loc_2 * Int_Loc_1;
      Int_Loc_2 = Int_Loc_3 / Int_Loc_1;
      Int_Loc_2 = 7 * (Int_Loc_3 - Int_Loc_2) - Int_Loc_1;

      Int_Loc_1_Ref_0 = Int_Loc_1;
      Proc_2(Int_Loc_1_Ref_0);
      Int_Loc_1 = Int_Loc_1_Ref_0;
      //Run_Index = Run_Index + 1;

      executeRuns(--Number_Of_Runs);
    }

  }

  void Proc_1(Record_Type Pointer_Par_Val) {
  /**
   * true
   *   if (types([ret/[void],this/[examples.Dhrystone],arg(1)/[examples.Record_Type]]))  {
   *        types([ret/[void],this/[examples.Dhrystone],arg(1)/[examples.Record_Type]])
   *   }   * true
   *   if (null([ret]) && any([this,arg(1)]))  {
   *        null([ret]) && any([this,arg(1)])
   *   }   * true
   *   if (arg(1)/top && this/top && ret/top)  {
   *        arg(1)/top && this/top && ret/top && size(ub,ret,1) && size(ub,this,size(this)) && size(ub,arg(1),size(arg(1)))
   *   }
   *  && cost(ub,ENERGY_CONSUMED,2123)
   */

    Record_Type Next_Record = Pointer_Par_Val.Record_Comp;
    Record_Type Record_Glob = null;
    Pointer_Par_Val.Record_Comp = Record_Glob;

    Pointer_Par_Val.Int_Comp = 5;

    Next_Record.Int_Comp = Pointer_Par_Val.Int_Comp;
    Next_Record.Record_Comp = Pointer_Par_Val.Record_Comp;
    Proc_3(Next_Record.Record_Comp);

    int Int_Ref_0;

    if (Next_Record.Discr == 1) {
      Next_Record.Int_Comp = 6;
      Int_Ref_0 = Next_Record.Enum_Comp;
      Proc_6(Pointer_Par_Val.Enum_Comp, Int_Ref_0);
      Next_Record.Enum_Comp = Int_Ref_0;
      Next_Record.Record_Comp = Record_Glob.Record_Comp;
      Int_Ref_0 = Next_Record.Int_Comp;
      Proc_7(Next_Record.Int_Comp, 10, Int_Ref_0);
      Next_Record.Int_Comp = Int_Ref_0;
    } else {
      Pointer_Par_Val = Pointer_Par_Val.Record_Comp;
    }
  }

  void Proc_2(int Int_Par_Ref_0) {
  /**
   * true
   *   if (types([ret/[void],this/[examples.Dhrystone],arg(1)/[int]]))  {
   *        types([ret/[void],this/[examples.Dhrystone],arg(1)/[int]])
   *   }   * true
   *   if (null([ret,arg(1)]) && any([this]))  {
   *        null([ret,arg(1)]) && any([this])
   *   }   * true
   *   if (arg(1)/top && this/top && ret/top)  {
   *        arg(1)/top && this/top && ret/top && size(ub,ret,1) && size(ub,this,size(this)) && size(ub,arg(1),int(arg(1)))
   *   }
   *  && cost(ub,ENERGY_CONSUMED,63)
   */

    char Char_Glob_1 = 'b';
    int Int_Loc;
    int Int_Glob = 10;

    Int_Loc = Int_Par_Ref_0 + 10;
    int Enum_Loc = 0;

    if (Char_Glob_1 == 'A') {
      Int_Loc -= 1;
      Int_Par_Ref_0 = Int_Loc - Int_Glob;
      Enum_Loc = 1;
    }
// old version of the if above
//    do if (Char_Glob_1 == 'A') {
//      Int_Loc -= 1;
//      Int_Par_Ref[0] = Int_Loc - Int_Glob;
//      Enum_Loc = 1;
//    } while (Enum_Loc != 1);

  }

  /**
   * true
   *   if (types([ret/[void],this/[examples.Dhrystone],arg(1)/[examples.Record_Type]]))  {
   *        types([ret/[void],this/[examples.Dhrystone],arg(1)/[examples.Record_Type]])
   *   }   * true
   *   if (null([ret]) && any([this,arg(1)]))  {
   *        null([ret]) && any([this,arg(1)])
   *   }   * true
   *   if (arg(1)/top && this/top && ret/top)  {
   *        arg(1)/top && this/top && ret/top && size(ub,ret,1) && size(ub,this,size(this)) && size(ub,arg(1),size(arg(1)))
   *   }
   *  && cost(ub,ENERGY_CONSUMED,541)
   */
  void Proc_3(Record_Type Pointer_Par_Ref) {
    Record_Type Record_Glob = null;
    int Int_Glob = 4;
    if (Record_Glob != null) {
      Pointer_Par_Ref = Record_Glob.Record_Comp;
    } else Int_Glob = 100;

    int Int_Comp_Ref_0 = Record_Glob.Int_Comp;
    Proc_7(10, Int_Glob, Int_Comp_Ref_0);
    Record_Glob.Int_Comp = Int_Comp_Ref_0;

  }

  /**
   * true
   *   if (types([ret/[void],this/[examples.Dhrystone]]))  {
   *        types([ret/[void],this/[examples.Dhrystone]])
   *   }   * true
   *   if (null([ret]) && any([this]))  {
   *        null([ret]) && any([this])
   *   }   * true
   *   if (this/top && ret/top)  {
   *        this/top && ret/top && size(ub,ret,1) && size(ub,this,size(this))
   *   }
   *  && cost(ub,ENERGY_CONSUMED,63)
   */
  void Proc_4() {
    char Char_Glob_1 = 'b';
    boolean Bool_Loc = Char_Glob_1 == 'A';
    char Char_Glob_2 = 'B';

  }

  /**
   * true
   *   if (types([ret/[void],this/[examples.Dhrystone]]))  {
   *        types([ret/[void],this/[examples.Dhrystone]])
   *   }   * true
   *   if (null([ret]) && any([this]))  {
   *        null([ret]) && any([this])
   *   }   * true
   *   if (this/top && ret/top)  {
   *        this/top && ret/top && size(ub,ret,1) && size(ub,this,size(this))
   *   }
   *  && cost(ub,ENERGY_CONSUMED,0)
   */
  void Proc_5() {
    char Char_Glob_1 = 'A';
    boolean Bool_Glob = false;
  }

  void Proc_6(int Enum_Par_Val, int Enum_Par_Ref) {

// Commented by Mario
//    Enum_Par_Ref[0] = Enum_Par_Val;
//
//    if (!Func_3(Enum_Par_Val)) Enum_Par_Ref[0] = 3;
//
//    switch (Enum_Par_Val) {
//
//      case 1:
//        Enum_Par_Ref[0] = 1;
//        break;
//
//      case 2:
//        if (Int_Glob > 100) Enum_Par_Ref[0] = 1;
//        else Enum_Par_Ref[0] = 3;
//        break;
//
//      case 3:
//        Enum_Par_Ref[0] = 2;
//        break;
//
//      case 3:
//        break;
//
//      case 4:
//        Enum_Par_Ref[0] = 3;
//        break;
//
//    }
  /**
   * true
   *   if (types([ret/[void],this/[examples.Dhrystone],arg(1)/[int],arg(2)/[int]]))  {
   *        types([ret/[void],this/[examples.Dhrystone],arg(1)/[int],arg(2)/[int]])
   *   }   * true
   *   if (null([ret]) && any([this,arg(1),arg(2)]))  {
   *        null([ret]) && any([this,arg(1),arg(2)])
   *   }   * true
   *   if (arg(2)/top && arg(1)/top && this/top && ret/top)  {
   *        arg(2)/top && arg(1)/top && this/top && ret/top && size(ub,ret,1) && size(ub,this,size(this)) && size(ub,arg(1),int(arg(1))) && size(ub,arg(2),int(arg(2)))
   *   }
   *  && cost(ub,ENERGY_CONSUMED,0)
   */

  }

  /**
   * true
   *   if (types([ret/[void],this/[examples.Dhrystone],arg(1)/[int],arg(2)/[int],arg(3)/[int]]))  {
   *        types([ret/[void],this/[examples.Dhrystone],arg(1)/[int],arg(2)/[int],arg(3)/[int]])
   *   }   * true
   *   if (null([ret,arg(2)]) && any([this,arg(1),arg(3)]))  {
   *        null([ret]) && any([this,arg(1),arg(2),arg(3)])
   *   }   * true
   *   if (arg(3)/top && arg(2)/top && arg(1)/top && this/top && ret/top)  {
   *        arg(3)/top && arg(2)/top && arg(1)/top && this/top && ret/top && size(ub,ret,1) && size(ub,this,size(this)) && size(ub,arg(1),int(arg(1))) && size(ub,arg(2),int(arg(2))) && size(ub,arg(3),int(arg(3)))
   *   }
   *  && cost(ub,ENERGY_CONSUMED,0)
   */
  void Proc_7(int Int_Par_Val1, int Int_Par_Val2, int Int_Par_Ref) {
    Int_Par_Ref = Int_Par_Val2 + Int_Par_Val1 + 2;
  }

  void Proc_8(int[] Array_Par_1_Ref, int[] Array_Par_2_Ref, int Int_Par_Val_1, int Int_Par_Val_2) {

    int Int_Index,
        Int_Loc;
  /**
   * true
   *   if (types([ret/[void],this/[examples.Dhrystone],arg(1)/[int],arg(2)/[int],arg(3)/[int],arg(4)/[int]]))  {
   *        types([ret/[void],this/[examples.Dhrystone],arg(1)/[int],arg(2)/[int],arg(3)/[int],arg(4)/[int]])
   *   }   * true
   *   if (null([ret,arg(3)]) && any([this,arg(1),arg(2),arg(4)]))  {
   *        null([ret,arg(3)]) && not_null([arg(1),arg(2)]) && any([this,arg(4)])
   *   }   * true
   *   if (arg(4)/top && arg(3)/top && arg(2)/top && arg(1)/top && this/top && ret/top)  {
   *        arg(4)/top && arg(3)/top && arg(2)/top && arg(1)/top && this/top && ret/top && size(ub,ret,1) && size(ub,this,size(this)) && size(ub,arg(1),size(arg(1))) && size(ub,arg(2),size(arg(2))) && size(ub,arg(3),int(arg(3))) && size(ub,arg(4),int(arg(4)))
   *   }
   *  && cost(ub,ENERGY_CONSUMED,1155)
   */

    Int_Loc = Int_Par_Val_1 + 5;
    Array_Par_1_Ref[Int_Loc] = Int_Par_Val_2;
    Array_Par_1_Ref[Int_Loc + 1] = Array_Par_1_Ref[Int_Loc];
    Array_Par_1_Ref[Int_Loc + 30] = Int_Loc;

//    for (Int_Index = Int_Loc; Int_Index <= Int_Loc + 1; ++Int_Index)
//      Array_Par_2_Ref[Int_Loc][Int_Index] = Int_Loc;
// the following 2 lines are equivalent to the above two lines
    Array_Par_2_Ref[Int_Loc] = Int_Loc;
    Array_Par_2_Ref[Int_Loc + 1] = Int_Loc;

    Array_Par_2_Ref[Int_Loc - 1] += 1;
    Array_Par_2_Ref[Int_Loc + 20] = Array_Par_1_Ref[Int_Loc];
//    Int_Glob = 5;
  }

  int Func_1(char Char_Par_1_Val, char Char_Par_2_Val) {
    char Char_Loc_1,
  /**
   * true
   *   if (types([ret/[int],this/[examples.Dhrystone],arg(1)/[char],arg(2)/[char]]))  {
   *        types([ret/[int],this/[examples.Dhrystone],arg(1)/[char],arg(2)/[char]])
   *   }   * true
   *   if (null([ret,arg(1),arg(2)]) && any([this]))  {
   *        null([ret,arg(1),arg(2)]) && any([this])
   *   }   * true
   *   if (arg(2)/top && arg(1)/top && this/top && ret/top)  {
   *        arg(2)/top && arg(1)/top && this/top && ret/top && size(ub,ret,2) && size(ub,this,size(this)) && size(ub,arg(1),size(arg(1))) && size(ub,arg(2),size(arg(2)))
   *   }
   *  && cost(ub,ENERGY_CONSUMED,122)
   */
        Char_Loc_2;
    Char_Loc_1 = Char_Par_1_Val;
    Char_Loc_2 = Char_Loc_1;
    if (Char_Loc_2 != Char_Par_2_Val) return 1;
    else return 2;

  }

// Commented by Jorge
//   boolean Func_2(String String_Par_1_Ref, String String_Par_2_Ref) {

//     int Int_Loc;
//     char Char_Loc = '\0';

//     Int_Loc = 2;
//     if (Func_1(String_Par_1_Ref.charAt(Int_Loc), String_Par_2_Ref.charAt(Int_Loc + 1)) == 1) {
//       Char_Loc = 'A';
//       Int_Loc += 1;
//     }
//     if (Char_Loc >= 'W' && Char_Loc < 'Z') Int_Loc = 7;
//     if (Char_Loc == 'X') return true;
//     else {
//       if (String_Par_1_Ref.compareTo(String_Par_2_Ref) > 0) {
//         Int_Loc += 7;
//         return true;
//       } else return false;
//     }

//   }

// Commented by Jorge
//   boolean Func_3(int Enum_Par_Val) {
//     return Enum_Par_Val == 3;
//   }

  public static void main(String argv[]) {
//    Msg.out = System.err;
//    Msg.out.println("Dhrystone Benchmark, Version 2.1 (Language: Java)");
//    Msg.out.println();
//    Msg.out.print("Please give the number of runs through the benchmark: ");
  /**
   * true
   *   if (types([ret/[void],arg(1)/[java.lang.String]]))  {
   *        types([ret/[void],arg(1)/[java.lang.String]])
   *   }   * true
   *   if (null([ret]) && any([arg(1)]))  {
   *        null([ret]) && any([arg(1)])
   *   }   * true
   *   if (arg(1)/top && ret/top)  {
   *        arg(1)/top && ret/top && size(ub,ret,1) && size(ub,arg(1),size(arg(1)))
   *   }
   *  && cost(ub,ENERGY_CONSUMED,496605435)
   */
//    Msg.out.flush();
    Dhrystone dh = new Dhrystone();
//    try {
//      BufferedReader rdr = new BufferedReader(new InputStreamReader(System.in));
//      dh.Number_Of_Runs = Integer.parseInt(rdr.readLine());
//    } catch (Exception e) {
//      e.printStackTrace();
//      return;
//    }
    dh.execute();
  }

//  /**
//   * java.lang.Runnable stuff
//   */
//  public void run() {
//    execute();
//    if (exitObserver != null) {
//      exitObserver.exitNotify();
//    }
//  }
//
//  public void setExitObserver(ExitObserver eo) {
//    exitObserver = eo;
//  }
//
}

//
//class ExitObserver {
//
//  public void exitNotify() {
//  }
//}
//
//class Msg extends OutputStream {
//  TextArea textarea;
//  public static PrintStream out;
//
//  Msg(TextArea ta) {
//    textarea = ta;
//    out = new PrintStream(this, true);
//  }
//
//  /**
//   * java.io.OutputStream stuff
//   */
//  public void write(int b) throws IOException {
//    if (textarea == null) {
//      System.out.write(b);
//    } else {
//      textarea.append("" + (char) b);
//    }
//  }
//}

class Record_Type {

  Record_Type Record_Comp;
  int Discr;
  int Enum_Comp;
  int Int_Comp;
  String String_Comp;
  int Enum_Comp_2;
  String String_Comp_2;
  char Char_Comp_1;
  char Char_Comp_2;
}
