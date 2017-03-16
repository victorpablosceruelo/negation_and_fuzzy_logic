class Labeling : public Space {
private:
  /// Alphabet has 26 letters
  static const int n = 7;
  /// Array for letters
  IntVarArray le;
public:
  /// Actual model
  Labeling(void) : le(*this,n,1,n) {
    branch(*this, le, INT_VAR_NONE, INT_VAL_MIN);

  }

  /// Constructor for cloning \a s
  Labeling(bool share, Labeling& s) : Space(share,s) {
    le.update(*this, share, s.le);
  }
  /// Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Labeling(share,*this);
  }
  /// Print solution
  virtual void
  print(std::ostream& os) const {
    os << "\t";
    os << std::endl;
  }
};
