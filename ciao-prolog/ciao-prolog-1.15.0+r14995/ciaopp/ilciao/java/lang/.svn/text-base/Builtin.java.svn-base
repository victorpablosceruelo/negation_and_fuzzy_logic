package lang;

import static soot.resources.Resource.ANY_RESOURCE;
import static soot.resources.Resource.ENERGY_CONSUMED;
import soot.resources.annotations.Cost;
import soot.resources.annotations.Measure;
import static soot.resources.annotations.Measure.MeasureType.IGNORE;
import soot.resources.annotations.Size;

@Cost(
    resources = {ANY_RESOURCE},
    functions = {"0"})
public class Builtin {
  ///////////////////////////////////////////////////////////////////////////////
  //                      IN ALPHABETICAL ORDER !!!!!!!!!!!!!!!!!!!
  ///////////////////////////////////////////////////////////////////////////////


  @Cost(
      resources = {ENERGY_CONSUMED},
      functions = {"15"})
  @Size("[+($(0,3),$(0,5)), void, $(0,3), void, $(0,5), void]")
  public static native int add(@Measure(IGNORE)Object typeRet, int lop,
      @Measure(IGNORE)Object typeLop, int rop, @Measure(IGNORE)Object typeRop);

  @Cost(
      resources = {ENERGY_CONSUMED},
      functions = {"9"})
  public static native Object asg(@Measure(IGNORE)Object typeRet, Object rop,
      @Measure(IGNORE)Object typeRop);

  @Cost(
      resources = {ENERGY_CONSUMED},
      functions = {"9"})
  @Size("[$(0,3), void, $(0,3), void]")
  public static native int asg_int(@Measure(IGNORE)Object typeRet, int rop,
      @Measure(IGNORE)Object typeRop);

  @Cost(
      resources = {ENERGY_CONSUMED},
      functions = {"9"})
  @Size("[$(0,3), void, $(0,3), void]")
  public static native Object asg_size(@Measure(IGNORE)Object typeRet, Object rop,
      @Measure(IGNORE)Object typeRop);

  @Cost(
      resources = {ENERGY_CONSUMED},
      functions = {"4"})
  @Size("[$(0,3), void, $(0,3), void]")
  public static native Object cast(@Measure(IGNORE)Object typeRet, Object rop,
      @Measure(IGNORE)Object typeRop);

  @Cost(
      resources = {ENERGY_CONSUMED},
      functions = {"23"})
  @Size("[/($(0,3),$(0,5)), void, $(0,3), void, $(0,5), void]")
  public static native int div(@Measure(IGNORE)Object typeRet, int lop,
      @Measure(IGNORE)Object typeLop, int rop, @Measure(IGNORE)Object typeRop);

  @Cost(
      resources = {ENERGY_CONSUMED},
      functions = {"13"})
  @Measure(retMeasure = IGNORE)
  public static native void eq(Object lop, @Measure(IGNORE)Object typeLop, Object rop,
      @Measure(IGNORE)Object typeRop);

  @Cost(
      resources = {ENERGY_CONSUMED},
      functions = {"13"})
  @Size("[void, $(0,2), void, $(0,4), void]")
  @Measure(retMeasure = IGNORE)
  public static native void eq_int(int lop, @Measure(IGNORE)Object typeLop, int rop,
      @Measure(IGNORE)Object typeRop);

  @Cost(
      resources = {ENERGY_CONSUMED},
      functions = {"13"})
  @Size("[void, $(0,2), void, $(0,4), void]")
  @Measure(retMeasure = IGNORE)
  public static native void eq_size(Object lop, @Measure(IGNORE)Object typeLop, Object rop,
      @Measure(IGNORE)Object typeRop);

  @Cost(
      resources = {ENERGY_CONSUMED},
      functions = {"15"})
  @Size("[void, $(0,2), void, $(0,4), void ]")
  public static native void ge(int lop, @Measure(IGNORE)Object typeLop, int rop,
      @Measure(IGNORE)Object typeRop);

  @Cost(
      resources = {ENERGY_CONSUMED},
      functions = {"15"})
  @Size("[void, $(0,2), void, $(0,4), void ]")
  public static native void gt(int lop, @Measure(IGNORE)Object typeLop, int rop,
      @Measure(IGNORE)Object typeRop);

  @Cost(
      resources = {ENERGY_CONSUMED},
      functions = {"20"} // ??
  )
  @Size("[$(0,1), void, $(0,3), $(0,4), void]")
  public static native Object gta(Object typeRet, Object base, int index, Object typeRef);

  @Cost(
      resources = {ENERGY_CONSUMED},
      functions = {"20"})
  @Size("[-($(0,3),1), void, $(0,3), void, void, void]")
  public static native Object gtf(@Measure(IGNORE)Object typeRet, Object rop,
      @Measure(IGNORE)Object typeRop, @Measure(IGNORE)Object nameField,
      @Measure(IGNORE)Object typeField);

  @Cost(
      resources = {ENERGY_CONSUMED},
      functions = {"15"})
  @Size("[void, $(0,4), void, $(0,4), void ]")
  public static native void le(int lop, @Measure(IGNORE)Object typeLop, int rop,
      @Measure(IGNORE)Object typeRop);

  @Cost(
      resources = {ENERGY_CONSUMED},
      functions = {"15"})
  @Size("[void, $(0,4), void, $(0,4), void ]")
  public static native void lt(int lop, @Measure(IGNORE)Object typeLop, int rop,
      @Measure(IGNORE)Object typeRop);

  @Cost(
      resources = {ENERGY_CONSUMED},
      functions = {"15"})
  @Size("[*($(0,3),$(0,5)), void, $(0,3), void, $(0,5), void]")
  public static native int mul(@Measure(IGNORE)Object typeRet, int lop,
      @Measure(IGNORE)Object typeLop, int rop, @Measure(IGNORE)Object typeRop);

  @Cost(
      resources = {ENERGY_CONSUMED},
      functions = {"13"})
  @Measure(retMeasure = IGNORE)
  public static native void ne(Object lop, @Measure(IGNORE)Object typeLop, Object rop,
      @Measure(IGNORE)Object typeRop);

  @Cost(
      resources = {ENERGY_CONSUMED},
      functions = {"13"})
  @Size("[void, $(0,2), void, $(0,4), void]")
  @Measure(retMeasure = IGNORE)
  public static native void ne_int(int lop, @Measure(IGNORE)Object typeLop, int rop,
      @Measure(IGNORE)Object typeRop);

  @Cost(
      resources = {ENERGY_CONSUMED},
      functions = {"13"})
  @Size("[void, $(0,2), void, $(0,4), void]")
  @Measure(retMeasure = IGNORE)
  public static native void ne_size(Object lop, @Measure(IGNORE)Object typeLop, Object rop,
      @Measure(IGNORE)Object typeRop);

  @Cost(
      resources = {ENERGY_CONSUMED},
      functions = {"93"})
  @Size("[$(0,1), $(0,2)]")
  public static native Object newa(Object typeRet, int size);

  @Cost(
      resources = {ENERGY_CONSUMED},
      functions = {"19"})
  //@Size("[1, $(0,2)]")
  @Size("[$(0,1), $(0,2)]")
  public static native Object newb(Object typeRet);

  @Cost(
      resources = {ENERGY_CONSUMED},
      functions = {"23"})
  @Size("[-($(0,5),1) , void, $(0,3), void, $(0,5), void]")
  public static native int rem(@Measure(IGNORE)Object typeRet, int lop,
      @Measure(IGNORE)Object typeLop, int rop, @Measure(IGNORE)Object typeRop);

  @Cost(
      resources = {ENERGY_CONSUMED},
      functions = {"15"})
  public static native Object shl(Object typeRet, Object lop, Object typeLop, Object rop,
      Object typeRop);

  @Cost(
      resources = {ENERGY_CONSUMED},
      functions = {"15"})
  public static native Object shr(Object typeRet, Object lop, Object typeLop, Object rop,
      Object typeRop);

  @Cost(
      resources = {ENERGY_CONSUMED},
      functions = {"20"} //
  )
  public static native Object sta(int pos, Object typeRef, Object rop, Object typeRop);

  @Cost(
      resources = {ENERGY_CONSUMED},
      functions = {"20"})
  @Size("[+($(0,2),$(0,6)), $(0,2), void, void, void, $(0,6), void]")
  public static native Object stf(Object lop, @Measure(IGNORE)Object typeLop,
      @Measure(IGNORE)Object fieldName, @Measure(IGNORE)Object fieldType, Object rop,
      @Measure(IGNORE)Object typeRop);

  @Cost(
      resources = {ENERGY_CONSUMED},
      functions = {"15"})
  @Size("[-($(0,3),$(0,5)), void, $(0,3), void, $(0,5), void]")
  public static native int sub(@Measure(IGNORE)Object typeRet, int lop,
      @Measure(IGNORE)Object typeLop, int rop, @Measure(IGNORE)Object typeRop);

  public static native void unk(@Measure(IGNORE)String statement);
}

// NOTES about "EnergyConsumed" resource: This resource represents the
// energy consumption of an embedded Java Virtual Machine measured in
// microjoules. Note that the main aim of the resource is not to develop a
// complete energy consumption model since since more information is really
// needed (bytecode instructions, length of arrays, etc). Instead, it is a
// proof of how we can use the resource analysis to perform this kind of
//   The upper bounds have been obtained from
// http://www.abo.fi/~slafond/javacosts and the paper "An Energy
// Consumption Model for an Embedded Java Virtual Machine" by S.Lafond and
// J.Lilius. Basically, for each builtin we have considered all possible
// bytecode instructions involved and obtained an upper bound. 
