package soot.resources;

import soot.tagkit.Tag;

public class CostInDollarsTag implements Tag {

  String costInDollars;

  public CostInDollarsTag(String costInDollars) {
    this.costInDollars = costInDollars;
  }

  public String toString() {
    return "CostInDollars";
  }

  public String getName() {
    return "CostInDollarsTag";
  }

  public String getInfo() {
    return "CostInDollars";
  }

  public byte[] getValue() {
    throw new RuntimeException("CostInDollarsTag has no value for bytecode");
  }

  public String getCostInDollars() {
    return costInDollars;
  }
}
