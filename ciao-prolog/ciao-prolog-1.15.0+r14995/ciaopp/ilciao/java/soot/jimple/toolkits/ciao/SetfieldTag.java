package soot.jimple.toolkits.ciao;

import soot.Value;
import soot.tagkit.AttributeValueException;
import soot.tagkit.Tag;

public class SetfieldTag implements Tag {

  Value value;

  SetfieldTag(Value value) {
    this.value = value;
  }

  public String getName() {
    return "setfieldTag";
  }

  public byte[] getValue() throws AttributeValueException {
    return new byte[0];
  }

  public Value getAssignedValue() {
    return value;
  }
}
