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

    public String format(String s) {
      return s.trim();
    }
  }

  class UnicodeEncoder implements Encoder {

    @Measure(retMeasure = IGNORE)
    UnicodeEncoder() {
    }

    public String format(String s) {
      return java.net.URLEncoder.encode(s);
    }
  }
}


