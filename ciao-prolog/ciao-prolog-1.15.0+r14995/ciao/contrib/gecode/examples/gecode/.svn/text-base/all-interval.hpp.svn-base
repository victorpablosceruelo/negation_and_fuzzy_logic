//11 defines the search space!

class AllInterval : public Space {
private:
  static const int size = 12;
  /// The numbers
  IntVarArray x;
public:
  /// Actual model
  AllInterval(void) :
    x(*this, size, 0, size - 1) {
    const int n = x.size();

    IntVarArgs d(n-1);

    // Set up variables for distance
    for (int i=0; i<n-1; i++)
      d[i] = expr(*this, abs(x[i+1]-x[i]));

    // Constrain them to be between 1 and n-1
    dom(*this, d, 1, n-1);

    distinct(*this, x);
    distinct(*this, d);

    // Break mirror symmetry
    rel(*this, x[0], IRT_LE, x[1]);
    // Break symmetry of dual solution
    rel(*this, d[0], IRT_GR, d[n-2]);

    branch(*this, x, INT_VAR_NONE, INT_VAL_MIN);
  }
  /// Constructor for cloning \a e
  AllInterval(bool share, AllInterval& e)
    : Space(share, e) {
    x.update(*this, share, e.x);
  }
  /// Copy during cloning
  virtual Space*
  copy(bool share) {
    return new AllInterval(share, *this);
  }
  /// Print solution
  virtual void
  print(std::ostream& os) const {
    const int n = x.size();
    os << "\tx[" << n << "] = {";
    for (int i = 0; i < n-1; i++)
      os << x[i] << "(" << abs(x[i+1].val()-x[i].val()) << "),";
    os << x[n-1] << "}" << std::endl;
  }
};
