package soot.resources.annotations;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

@Retention(RetentionPolicy.RUNTIME)
public @interface Measure {

  public static enum MeasureType {
    DEFAULT {
      public String getMeasure() {
        throw new RuntimeException("Tried to retrieve the measure of a default annotation.");
      }
    },
    INTEGER {
      public String getMeasure() {
        return "int";
      }
    },
    IGNORE {
      public String getMeasure() {
        return "void";
      }
    },
    SIZE {
      public String getMeasure() {
        return "size";
      }
    };

    public abstract String getMeasure();
  }

  MeasureType retMeasure() default MeasureType.DEFAULT;

  MeasureType thisMeasure() default MeasureType.DEFAULT;

  MeasureType value() default MeasureType.DEFAULT;
}

