
package examples;

import annot_java.lang.Stream;
import static soot.resources.Resource.COST_IN_DOLLARS;
import soot.resources.annotations.Measure;
import static soot.resources.annotations.Measure.MeasureType.IGNORE;
import soot.resources.annotations.Resources;

@Resources({COST_IN_DOLLARS})
public class CellPhone {

  @Measure(
      retMeasure = IGNORE)
  public CellPhone() {
  }

  /**
   * true
   *   if (types([ret/[examples.CellPhone$SmsPacket],this/[examples.CellPhone],arg(1)/[examples.CellPhone$SmsPacket],arg(2)/[examples.CellPhone$Encoder],arg(3)/[annot_java.lang.Stream]]))  {
   *        types([ret/[examples.CellPhone$SmsPacket],this/[examples.CellPhone],arg(1)/[examples.CellPhone$SmsPacket],arg(2)/[examples.CellPhone$Encoder],arg(3)/[annot_java.lang.Stream]])
   *   }   * true
   *   if (any([ret,this,arg(1),arg(2),arg(3)]))  {
   *        null([ret]) && any([this,arg(1),arg(2),arg(3)])
   *   }   * true
   *   if (arg(3)/top && arg(2)/top && arg(1)/top && this/top && ret/top)  {
   *        arg(3)/top && arg(2)/top && arg(1)/top && this/top && ret/top && size(ub,ret,3.5*exp(size(arg(1)),2)-2.5*size(arg(1))) && size(ub,this,size(this)) && size(ub,arg(1),size(arg(1))) && size(ub,arg(2),size(arg(2))) && size(ub,arg(3),size(arg(3)))
   *   }
   *  && cost(ub,COST_IN_DOLLARS,6.0*exp(size(arg(1)),2)-6.0*size(arg(1)))
   */
  public SmsPacket sendSms(SmsPacket smsPk, Encoder enc, Stream stm) {
    if (smsPk != null) {
      String newSms = enc.format(smsPk.sms);
      stm.send(newSms);
      smsPk.next = sendSms(smsPk.next, enc, stm);
      smsPk.sms = newSms;
      return smsPk;
    } else {
      return null;
    }
  }

  class SmsPacket {
    String sms;
    SmsPacket next;
  }

  interface Encoder {
    String format(String data);
  }

  class TrimEncoder implements Encoder {

    @Measure(retMeasure = IGNORE)
    TrimEncoder() {
    }

  /**
   * true
   *   if (types([ret/[java.lang.String],this/[examples.CellPhone$TrimEncoder],arg(1)/[java.lang.String]]))  {
   *        types([ret/[java.lang.String],this/[examples.CellPhone$TrimEncoder],arg(1)/[java.lang.String]])
   *   }   * true
   *   if (any([ret,this,arg(1)]))  {
   *        any([this,arg(1)])
   *   }   * true
   *   if (arg(1)/top && this/top && ret/top)  {
   *        arg(1)/top && this/top && ret/top && size(ub,ret,size(arg(1))) && size(ub,this,size(this)) && size(ub,arg(1),size(arg(1)))
   *   }
   *  && cost(ub,COST_IN_DOLLARS,0)
   */
    public String format(String s) {
      return s.trim();
    }
  }

  class UnicodeEncoder implements Encoder {

    @Measure(retMeasure = IGNORE)
    UnicodeEncoder() {
    }

  /**
   * true
   *   if (types([ret/[java.lang.String],this/[examples.CellPhone$UnicodeEncoder],arg(1)/[java.lang.String]]))  {
   *        types([ret/[java.lang.String],this/[examples.CellPhone$UnicodeEncoder],arg(1)/[java.lang.String]])
   *   }   * true
   *   if (any([ret,this,arg(1)]))  {
   *        any([this,arg(1)])
   *   }   * true
   *   if (arg(1)/top && this/top && ret/top)  {
   *        arg(1)/top && this/top && ret/top && size(ub,ret,6*size(arg(1))) && size(ub,this,size(this)) && size(ub,arg(1),size(arg(1)))
   *   }
   *  && cost(ub,COST_IN_DOLLARS,0)
   */
    public String format(String s) {
      return java.net.URLEncoder.encode(s);
    }
  }
}


