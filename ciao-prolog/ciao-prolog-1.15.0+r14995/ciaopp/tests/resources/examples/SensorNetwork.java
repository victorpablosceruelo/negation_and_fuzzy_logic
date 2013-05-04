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
public class SensorNetwork {

  
  public SensorNetwork() {
  }

  public annot_java.lang.StringBuffer collectData(int i, Sensor sensors[]) {
      if (i == 0)
	  return new annot_java.lang.StringBuffer();
      else{
	  java.lang.String data = sensors[i].read();	
	  i = i-1;
	  annot_java.lang.StringBuffer buffer = collectData(i,sensors);
	  return buffer.append(data);
      }
  }

  interface Sensor {
    java.lang.String read();
  }


    // We assume that the size of measurement obtained from a seismic
    // sensor are twice than the measurements from a temperature sensor. Also,
    // we are assuming that read from a temperature sensor consumes 30mj and from
    // a seismic sensor consumes 40mj. This amount are totally arbitrarily.
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


