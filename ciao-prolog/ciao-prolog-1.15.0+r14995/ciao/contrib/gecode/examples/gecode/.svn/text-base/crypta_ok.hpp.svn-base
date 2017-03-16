class Crypta : public Space {
private:
  static const int n = 10;
  IntVarArgs le;
public:
  /// Actual model
  Crypta(void) : le(*this,n+2,0,n-1) {
    IntVar
      a(le[ 0]), b(le[ 1]), c(le[ 2]), d(le[3]), e(le[ 4]),
      f(le[ 5]) ,g(le[ 6]), h(le[ 7]), i(le[ 8]), j(le[ 9]),
      sr1(le[10]), sr2(le[11]);

    rel(*this, b, IRT_GR, 0);
    rel(*this, d, IRT_GR, 0);
    rel(*this, g, IRT_GR, 0);

    rel(*this, sr1, IRT_LQ, 1);
    rel(*this, sr2, IRT_LQ, 1);

    rel(*this, a, IRT_NQ, b);
    rel(*this, a, IRT_NQ, c);
    rel(*this, a, IRT_NQ, d);
    rel(*this, a, IRT_NQ, e);
    rel(*this, a, IRT_NQ, f);
    rel(*this, a, IRT_NQ, g);
    rel(*this, a, IRT_NQ, h);
    rel(*this, a, IRT_NQ, i);
    rel(*this, a, IRT_NQ, j);

    rel(*this, f, IRT_NQ, g);
    rel(*this, f, IRT_NQ, h);
    rel(*this, f, IRT_NQ, i);
    rel(*this, f, IRT_NQ, j);

    rel(*this, g, IRT_NQ, h);
    rel(*this, g, IRT_NQ, i);
    rel(*this, g, IRT_NQ, j);

    rel(*this, h, IRT_NQ, i);
    rel(*this, h, IRT_NQ, j);

    rel(*this, i, IRT_NQ, j);

    rel(*this, e, IRT_NQ, f);
    rel(*this, e, IRT_NQ, g);
    rel(*this, e, IRT_NQ, h);
    rel(*this, e, IRT_NQ, i);
    rel(*this, e, IRT_NQ, j);

    rel(*this, d, IRT_NQ, e);
    rel(*this, d, IRT_NQ, f);
    rel(*this, d, IRT_NQ, g);
    rel(*this, d, IRT_NQ, h);
    rel(*this, d, IRT_NQ, i);
    rel(*this, d, IRT_NQ, j);

    rel(*this, c, IRT_NQ, d);
    rel(*this, c, IRT_NQ, e);
    rel(*this, c, IRT_NQ, f);
    rel(*this, c, IRT_NQ, g);
    rel(*this, c, IRT_NQ, h);
    rel(*this, c, IRT_NQ, i);
    rel(*this, c, IRT_NQ, j);

    rel(*this, b, IRT_NQ, c);
    rel(*this, b, IRT_NQ, d);
    rel(*this, b, IRT_NQ, e);
    rel(*this, b, IRT_NQ, f);
    rel(*this, b, IRT_NQ, g);
    rel(*this, b, IRT_NQ, h);
    rel(*this, b, IRT_NQ, i);
    rel(*this, b, IRT_NQ, j);

//     le << expr(*this,10*e);
//     le << expr(*this,100*j);
//     le << expr(*this,1000*b);
//     le << expr(*this,10000*b);
//     le << expr(*this,100000*e);
//     le << expr(*this,1000000*f);
//     le << expr(*this,10*j);
//     le << expr(*this,100*e);
//     le << expr(*this,1000*f);
//     le << expr(*this,10000*g);
//     le << expr(*this,100000*a);
//     le << expr(*this,1000000*f);
//     le << expr(*this,10*e);
//     le << expr(*this,100*e);
//     le << expr(*this,1000*h);
//     le << expr(*this,10000*i);
//     le << expr(*this,100000*f);
//     le << expr(*this,1000000*b);
//     le << expr(*this,10000000*sr1);

//     le << expr(*this,10*f);
//     le << expr(*this,100*h);
//     le << expr(*this,1000*a);
//     le << expr(*this,10000*i);
//     le << expr(*this,100000*i);
//     le << expr(*this,1000000*j);
//     le << expr(*this,10*i);
//     le << expr(*this,100*b);
//     le << expr(*this,1000*d);
//     le << expr(*this,10000*i);
//     le << expr(*this,100000*d);
//     le << expr(*this,1000000*c);
//     le << expr(*this,10*f);
//     le << expr(*this,100*a);
//     le << expr(*this,1000*f);
//     le << expr(*this,10000*h);
//     le << expr(*this,100000*d);
//     le << expr(*this,1000000*d);
//     le << expr(*this,10000000*sr2);

//     le << expr(*this,10*j);
//     le << expr(*this,100*j);
//     le << expr(*this,1000*i);
//     le << expr(*this,10000*a);
//     le << expr(*this,100000*b);
//     le << expr(*this,10*a);
//     le << expr(*this,100*g);
//     le << expr(*this,1000*f);
//     le << expr(*this,10000*h);
//     le << expr(*this,100000*d);
//     le << expr(*this,10*a);
//     le << expr(*this,100*g);
//     le << expr(*this,1000*e);
//     le << expr(*this,10000*j);
//     le << expr(*this,100000*g);

//     le << expr(*this,a+10*e);
//     le << expr(*this,le[le.size()-1]+100*j);
//     le << expr(*this,le[le.size()-1]+1000*b);
//     le << expr(*this,le[le.size()-1]+10000*b);
//     le << expr(*this,le[le.size()-1]+100000*e);
//     le << expr(*this,le[le.size()-1]+1000000*f);
//     le << expr(*this,le[le.size()-1]+e);
//     le << expr(*this,le[le.size()-1]+10*j);
//     le << expr(*this,le[le.size()-1]+100*e);
//     le << expr(*this,le[le.size()-1]+1000*f);
//     le << expr(*this,le[le.size()-1]+10000*g);
//     le << expr(*this,le[le.size()-1]+100000*a);
//     le << expr(*this,le[le.size()-1]+1000000*f);
//     le << expr(*this,f+10*e);
//     le << expr(*this,le[le.size()-1]+100*e);
//     le << expr(*this,le[le.size()-1]+1000*h);
//     le << expr(*this,le[le.size()-1]+10000*i);
//     le << expr(*this,le[le.size()-1]+100000*f);
//     le << expr(*this,le[le.size()-1]+1000000*b);
//     le << expr(*this,le[le.size()-1]+10000000*sr1);

//     le << expr(*this,c+10*f);
//     le << expr(*this,le[le.size()-1]+100*h);
//     le << expr(*this,le[le.size()-1]+1000*a);
//     le << expr(*this,le[le.size()-1]+10000*i);
//     le << expr(*this,le[le.size()-1]+100000*i);
//     le << expr(*this,le[le.size()-1]+1000000*j);
//     le << expr(*this,le[le.size()-1]+f);
//     le << expr(*this,le[le.size()-1]+10*i);
//     le << expr(*this,le[le.size()-1]+100*b);
//     le << expr(*this,le[le.size()-1]+1000*d);
//     le << expr(*this,le[le.size()-1]+10000*i);
//     le << expr(*this,le[le.size()-1]+100000*d);
//     le << expr(*this,le[le.size()-1]+1000000*c);
//     le << expr(*this,le[le.size()-1]+sr1);
//     le << expr(*this,j+10*f);
//     le << expr(*this,le[le.size()-1]+100*a);
//     le << expr(*this,le[le.size()-1]+1000*f);
//     le << expr(*this,le[le.size()-1]+10000*h);
//     le << expr(*this,le[le.size()-1]+100000*d);
//     le << expr(*this,le[le.size()-1]+1000000*d);
//     le << expr(*this,le[le.size()-1]+10000000*sr2);

//     le << expr(*this,a+10*j);
//     le << expr(*this,le[le.size()-1]+100*j);
//     le << expr(*this,le[le.size()-1]+1000*i);
//     le << expr(*this,le[le.size()-1]+10000*a);
//     le << expr(*this,le[le.size()-1]+100000*b);
//     le << expr(*this,le[le.size()-1]+b);
//     le << expr(*this,le[le.size()-1]+10*a);
//     le << expr(*this,le[le.size()-1]+100*g);
//     le << expr(*this,le[le.size()-1]+1000*f);
//     le << expr(*this,le[le.size()-1]+10000*h);
//     le << expr(*this,le[le.size()-1]+100000*d);
//     le << expr(*this,le[le.size()-1]+sr2);

//     le << expr(*this,c+10*a);
//     le << expr(*this,le[le.size()-1]+100*g);
//     le << expr(*this,le[le.size()-1]+1000*e);
//     le << expr(*this,le[le.size()-1]+10000*j);
//     le << expr(*this,le[le.size()-1]+100000*g);

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
  /// Print solution
  virtual void
  print(std::ostream& os) const {
    os << "\t" << le << std::endl;
  }
};
