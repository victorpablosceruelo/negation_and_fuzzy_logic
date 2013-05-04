class Crypta : public Space {
private:
  static const int n = 10;
  IntVarArgs le;
public:
  /// Actual model
  Crypta(void) : le(*this,n+2,0,n-1) {
    IntVar
      a(le[ 0]), b(le[ 1]), c(le[ 2]), d(le[3]), e(le[ 4]),
      f(le[ 5]) ,g(le[ 6]), h(le[ 7]), i(le[8]), j(le[ 9]),
      sr1(le[10]), sr2(le[11]);

     rel(*this, b, IRT_GR, 0);
     rel(*this, d, IRT_GR, 0);

     rel(*this, sr1, IRT_LQ, 1);
     rel(*this, sr2, IRT_LQ, 1);

      rel(*this, a+10*e+100*j+1000*b+10000*b+100000*e+1000000*f+
	  e+10*j+100*e+1000*f+10000*g+100000*a+1000000*f
	  == f+10*e+100*e+1000*h+10000*i+100000*f+1000000*b+10000000*sr1);
    
      rel(*this, c+10*f+100*h+1000*a+10000*i+100000*i+1000000*j+
	  f+10*i+100*b+1000*d+10000*i+100000*d+1000000*c+sr1
	  == j+10*f+100*a+1000*f+10000*h+100000*d+1000000*d+10000000*sr2);
    
      rel(*this, a+10*j+100*j+1000*i+10000*a+100000*b+
	  b+10*a+100*g+1000*f+10000*h+100000*d+sr2
	  == c+10*a+100*g+1000*e+10000*j+100000*g);

      branch(*this, le, INT_VAR_NONE, INT_VAL_MIN);

  }

  /// Constructor for cloning \a s
  Crypta(bool share, Crypta& s) : Space(share,s) {
     int i;
     IntVarArgs tmp(s.le.size());
     le = tmp;
     for (i = 0; i < le.size(); i++)
      le[i].update(*this, share, s.le[i]);
  }
  /// Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Crypta(share,*this);
  }

  void make_dist()  
  {
     IntVarArgs dist;
     dist << le[0];
     dist << le[1];
     dist << le[2];
     dist << le[3];
     dist << le[4];
     dist << le[5];
     dist << le[6];
     dist << le[7];
     dist << le[8];
     dist << le[9];
     distinct(*this, dist);
  }

  /// Print solution
  virtual void
  print(std::ostream& os) const {
    os << "\t" << le << std::endl;
  }
};
