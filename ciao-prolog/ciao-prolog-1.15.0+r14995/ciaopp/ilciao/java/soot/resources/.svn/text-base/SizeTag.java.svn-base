package soot.resources;

import soot.tagkit.Tag;

public class SizeTag implements Tag {

  String size;

  public SizeTag(String size) {
    this.size = size;
  }

  public String toString() {
    return "Size";
  }

  public String getName() {
    return "SizeTag";
  }

  public String getInfo() {
    return "Size";
  }

  public byte[] getValue() {
    throw new RuntimeException("SizeTag has no value for bytecode");
  }

  public String getSize() {
    return size;
  }
}
