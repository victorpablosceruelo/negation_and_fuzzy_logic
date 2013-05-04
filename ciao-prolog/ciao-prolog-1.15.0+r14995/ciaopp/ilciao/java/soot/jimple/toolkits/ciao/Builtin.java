package soot.jimple.toolkits.ciao;

public enum Builtin {
  ADD,
  ASG {
    public Builtin getSpecializedSizeVersion() {
      return ASG_SIZE;
    }

    public Builtin getSpecializedIntVersion() {
      return ASG_INT;
    }
  },
  ASG_INT,
  ASG_SIZE,
  CAST,
  DIV,
  EQ {
    public Builtin getSpecializedSizeVersion() {
      return EQ_SIZE;
    }

    public Builtin getSpecializedIntVersion() {
      return EQ_INT;
    }
  },
  EQ_INT,
  EQ_SIZE,
  GE,
  GT,
  GTF,
  GTA,
  IOF,
  LE,
  LT,
  MUL,
  NE {
    public Builtin getSpecializedSizeVersion() {
      return NE_SIZE;
    }

    public Builtin getSpecializedIntVersion() {
      return NE_INT;
    }
  },
  NE_INT,
  NE_SIZE,
  NEWB,
  NEWA,
  REM,
  SHL,
  SHR,
  STA,
  STF,
  SUB,
  UNK;

  public String getName() {
    return name().toLowerCase();
  }

  public Builtin getSpecializedSizeVersion() {
    return this;
  }

  public Builtin getSpecializedIntVersion() {
    return this;
  }
}
