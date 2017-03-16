package soot.resources.annotations;

import soot.resources.Resource;

import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

@Inherited
@Retention(RetentionPolicy.RUNTIME)
public @interface Cost {
  Resource[] resources();

  String[] functions();
}
