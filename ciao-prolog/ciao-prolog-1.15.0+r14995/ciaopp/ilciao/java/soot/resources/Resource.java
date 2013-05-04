package soot.resources;

import org.jetbrains.annotations.Nullable;
import soot.resources.annotations.Cost;

import java.util.Arrays;
import java.util.List;


public enum Resource {
  // RESPECT THE ALPHABETICAL ORDER !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ACCESSES_DB,
  ANY_RESOURCE,
  BYTES_RECEIVED,
  CLOSED_FILES,
  COST_IN_DOLLARS,
  DATA_STORED,
  ENERGY_CONSUMED{
    public ResourceFunction getResourceFunction() {
      return new EnergyConsumedFunction();
    }
  },
  HEAP_USAGE {
    public ResourceFunction getResourceFunction() {
      return new HeapUsageFunction();
    }
  },
  OPENED_FILES {
    public ResourceFunction getResourceFunction() {
      return new OpenedFilesFunction();
    }
  },
  SCREEN_WIDTH,
  SIZE {
    public ResourceFunction getResourceFunction() {
      return new SizeFunction();
    }
  },
  STACK_USAGE,
  STEPS {
    public ResourceFunction getResourceFunction() {
      return new StepsFunction();
    }
  };

  public ResourceFunction getResourceFunction() {
    return new GenericResourceFunction(this);
  }

  @Nullable
  String getFunction(Cost costAnnotation) {
    Resource[] res = costAnnotation.resources();
    List<Resource> resources = Arrays.asList(res);
    int index = resources.indexOf(this);
    String[] functions = costAnnotation.functions();
    //System.out.println("............Annotation " + costAnnotation);
    if (index >= 0) {
      if (functions.length - 1 < index) {
        throw new IndexOutOfBoundsException("The resources vector: " + res + " is larger" +
            " than the functions vector:" + functions);
      } else {
        return costAnnotation.functions()[index];
      }
    }
    return null;
  }
}
