
package examples;

import soot.resources.Resource;
import soot.resources.annotations.Resources;
import soot.resources.annotations.Measure;
import soot.resources.annotations.Cost;
import soot.resources.annotations.Size;
import annot_java.lang.Stream;
import static soot.resources.annotations.Measure.MeasureType.IGNORE;
import static soot.resources.Resource.ENERGY_CONSUMED;


@Resources({ENERGY_CONSUMED})
public class SensorNetworkLoop {

  
  public SensorNetworkLoop() {
  }

  public annot_java.lang.StringBuffer collectData(Sensor sensors[]) {
  /**
   * true
   *   if (types([ret/[annot_java.lang.StringBuffer],this/[examples.SensorNetworkLoop],arg(1)/[examples.SensorNetworkLoop$Sensor]]))  {
   *        types([ret/[annot_java.lang.StringBuffer],this/[examples.SensorNetworkLoop],arg(1)/[examples.SensorNetworkLoop$Sensor]])
   *   }   * true
   *   if (any([ret,this,arg(1)]))  {
   *        not_null([ret]) && any([this,arg(1)])
   *   }   * true
   *   if (arg(1)/top && this/top && ret/top)  {
   *        arg(1)/top && this/top && ret/top && size(ub,ret,1$1) && size(ub,this,size(this)) && size(ub,arg(1),size(arg(1)))
   *   }
   *  && cost(ub,ENERGY_CONSUMED,160*1$1+3851)
   */
      int i;
      int n = 8;
      annot_java.lang.StringBuffer buffer = new annot_java.lang.StringBuffer();
      for (i=n;i>0;i--){
	  java.lang.String data = sensors[i].read();	
	  buffer.append(data);
      }
      return buffer;      
  }

  interface Sensor {
    java.lang.String read();
  }

    // We assume that the cost of measurement obtained from a seismic
    // sensor are twice than the measurements from a temperature sensor. Also,
    // we are assuming that read from a temperature sensor consumes 10mj and from
    // a seismic sensor consumes 20mj. This amount are totally arbitrarily.
    // This assumptions are only to make more interesting the example.

  class TemperatureSensor implements Sensor {
      TemperatureSensor() {};

      @Size("[$(0,1),$(0,2),$(0,3)]")
      @Cost(
          resources = {Resource.ENERGY_CONSUMED},
          functions = {"*(10,$(0,1))"})
      public native java.lang.String read();

  }


  class SeismicSensor implements Sensor {
      SeismicSensor() {};

      @Size("[$(0,1),$(0,2),$(0,3)]")
      @Cost(
          resources = {Resource.ENERGY_CONSUMED},
          functions = {"*(20,$(0,1))"})
      public native java.lang.String read();
  }
}


