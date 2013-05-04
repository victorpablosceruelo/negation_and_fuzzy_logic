/* -------------------------- */
/*    General Gecode Space    */
/* -------------------------- */

class IGecode : public Space 
{
protected:
  //Model vars;
  IntVarArgs vars;
  //Temporal vars;
  IntVarArgs tmp_vars;

public:
  /// Actual model
  IGecode(void)  
  {
    tmp_vars << IntVar(*this,0,1);
  }

  /// Print solution
  virtual void print() const 
  {
    std::cout << "\t" << vars << std::endl;
  }

  //Constructor for cloning
  IGecode(bool share, IGecode& s) : Space(share,s) 
  {
    IntVarArgs tmp(s.vars.size());
    vars = tmp;
    int i;
    for (i = 0; i < vars.size(); i++)
      vars[i].update(*this, share, s.vars[i]);

    tmp_vars << IntVar(*this,0,1);
  }

  //Copy during cloning
  virtual Space* copy(bool share) 
  {
    return new IGecode(share,*this);
  }

  //Print a variable
  void print_variable(int id) 
  {
    std::cout << vars[id] << std::endl;
  }

  void put_branch(tagged_t list) 
  {
    IntVarArgs arg;
    for(;list != EMPTY_LIST; list = TailOfTerm(list))
      {
	arg << vars[GetInteger(HeadOfTerm(list))];
      }

    branch(*this, arg, INT_VAR_NONE, INT_VAL_MIN);
  }

  //Create a new variable
  int new_variable(int lb, int ub) 
  {
    int id = vars.size();
    vars << IntVar(*this,lb,ub);

    return id;
  }

  //Create a new variable
  int new_tmp_variable(int lb, int ub) 
  {
    int id = tmp_vars.size();
    tmp_vars << IntVar(*this,lb,ub);

    return id;
  }

  //Assigns an integer to a variable
  int equal_integer(int id1, int value) 
  {
    IntVar v;
    if (id1 < 0)  v = tmp_vars[id1*(-1)];
    else v = vars[id1];

    rel(*this, v, IRT_EQ, value);

    return TRUE;
  }

  //Assignation between variables
  int equal_variable(int id1, int id2) 
  {
    IntVar v1;
    if (id1 < 0)  v1 = tmp_vars[id1*(-1)];
    else v1 = vars[id1];

    IntVar v2;
    if (id2 < 0)  v2 = tmp_vars[id2*(-1)];
    else v2 = vars[id2];

    rel(*this, v1, IRT_EQ, v2);

    return TRUE;
  }

  //Constrains a variable to be less than an integer
  int less_integer(int id1, int value) 
  {
    IntVar v;
    if (id1 < 0)  v = tmp_vars[id1*(-1)];
    else v = vars[id1];

    rel(*this, v, IRT_LE, value);

    return TRUE;
  }

  //Constrains a variable to be less than another variable
  int less_variable(int id1, int id2) 
  {
    IntVar v1;
    if (id1 < 0)  v1 = tmp_vars[id1*(-1)];
    else v1 = vars[id1];

    IntVar v2;
    if (id2 < 0)  v2 = tmp_vars[id2*(-1)];
    else v2 = vars[id2];

    rel(*this, v1, IRT_LE, v2);

    return TRUE;
  }

  //Constrains a variable to be less or equal than an integer
  int less_equal_integer(int id1, int value) 
  {
    IntVar v;
    if (id1 < 0)  v = tmp_vars[id1*(-1)];
    else v = vars[id1];

    rel(*this, v, IRT_LQ, value);

    return TRUE;
  }

  //Constrains a variable to be less or equal than another variable
  int less_equal_variable(int id1, int id2) 
  {
    IntVar v1;
    if (id1 < 0)  v1 = tmp_vars[id1*(-1)];
    else v1 = vars[id1];

    IntVar v2;
    if (id2 < 0)  v2 = tmp_vars[id2*(-1)];
    else v2 = vars[id2];

    rel(*this, v1, IRT_LQ, v2);

    return TRUE;
  }

  //Constrains a variable to be greater than an integer
  int greater_integer(int id1, int value) 
  {
    IntVar v;
    if (id1 < 0)  v = tmp_vars[id1*(-1)];
    else v = vars[id1];

    rel(*this, v, IRT_GR, value);

    return TRUE;
  }

  //Constrains a variable to be greater than another variable
  int greater_variable(int id1, int id2) 
  {
    IntVar v1;
    if (id1 < 0)  v1 = tmp_vars[id1*(-1)];
    else v1 = vars[id1];

    IntVar v2;
    if (id2 < 0)  v2 = tmp_vars[id2*(-1)];
    else v2 = vars[id2];

    rel(*this, v1, IRT_GR, v2);

    return TRUE;
  }

  //Constrains a variable to be greater or equal than an integer
  int greater_equal_integer(int id1, int value) 
  {
    IntVar v;
    if (id1 < 0)  v = tmp_vars[id1*(-1)];
    else v = vars[id1];

    rel(*this, v, IRT_GQ, value);

    return TRUE;
  }

  //Constrains a variable to be greater or equal than another variable
  int greater_equal_variable(int id1, int id2) 
  {
    IntVar v1;
    if (id1 < 0)  v1 = tmp_vars[id1*(-1)];
    else v1 = vars[id1];

    IntVar v2;
    if (id2 < 0)  v2 = tmp_vars[id2*(-1)];
    else v2 = vars[id2];

    rel(*this, v1, IRT_GR, v2);

    return TRUE;
  }

  //Constrains a variable to be different than an integer
  int different_integer(int id1, int value) 
  {
    IntVar v;
    if (id1 < 0)  v = tmp_vars[id1*(-1)];
    else v = vars[id1];

    rel(*this, v, IRT_NQ, value);

    return TRUE;
  }

  //Constrains a variable to be different than another variable
  int different_variable(int id1, int id2) 
  {
    IntVar v1;
    if (id1 < 0)  v1 = tmp_vars[id1*(-1)];
    else v1 = vars[id1];

    IntVar v2;
    if (id2 < 0)  v2 = tmp_vars[id2*(-1)];
    else v2 = vars[id2];

    rel(*this, v1, IRT_NQ, v2);

    return status();
  }

  //Adds an integer to a variable
  int add_integer(int id1, int value) 
  {
    IntVar v;
    if (id1 < 0)  v = tmp_vars[id1*(-1)];
    else v = vars[id1];

    IntVar resVar = expr(*this, v + value);

    int res = tmp_vars.size() * (-1);
    tmp_vars << resVar;

    return res;
  }

  //Adds a variable to  another variable
  int add_variable(int id1, int id2) 
  {
    IntVar v1;
    if (id1 < 0)  v1 = tmp_vars[id1*(-1)];
    else v1 = vars[id1];

    IntVar v2;
    if (id2 < 0)  v2 = tmp_vars[id2*(-1)];
    else v2 = vars[id2];

    IntVar resVar = expr(*this, v1 + v2);

    int res = tmp_vars.size() * (-1);
    tmp_vars << resVar;

    return res;
  }

  //Substracts an integer from a variable
  int sub_integer(int id1, int value) 
  {
    IntVar v;
    if (id1 < 0)  v = tmp_vars[id1*(-1)];
    else v = vars[id1];

    IntVar resVar = expr(*this, v - value);

    int res = tmp_vars.size() * (-1);
    tmp_vars << resVar;

    return res;
  }

  //Substracts a variable from another variable
  int sub_variable(int id1, int id2) 
  {
    IntVar v1;
    if (id1 < 0)  v1 = tmp_vars[id1*(-1)];
    else v1 = vars[id1];

    IntVar v2;
    if (id2 < 0)  v2 = tmp_vars[id2*(-1)];
    else v2 = vars[id2];

    IntVar resVar = expr(*this, v1 - v2);

    int res = tmp_vars.size() * (-1);
    tmp_vars << resVar;

    return res;
  }

  //Multiplies an integer and a variable
  int mul_integer(int id1, int value) 
  {
    IntVar v;
    if (id1 < 0)  v = tmp_vars[id1*(-1)];
    else v = vars[id1];

    IntVar resVar = expr(*this, v * value);

    int res = tmp_vars.size() * (-1);
    tmp_vars << resVar;

    return res;
  }

  //Multiplies two variables
  int mul_variable(int id1, int id2) 
  {
    IntVar v1;
    if (id1 < 0)  v1 = tmp_vars[id1*(-1)];
    else v1 = vars[id1];

    IntVar v2;
    if (id2 < 0)  v2 = tmp_vars[id2*(-1)];
    else v2 = vars[id2];

    IntVar resVar = expr(*this, v1 * v2);

    int res = tmp_vars.size() * (-1);
    tmp_vars << resVar;

    return res;
  }

  //gets the lower bound of a variable
  int get_lb(int id) 
  {
    return vars[id].min();
  }

  //gets the upper bound of a variable
  int get_ub(int id) 
  {
    return vars[id].max();
  }

  //gets all the possible values of a variable
  int *get_values(int id) 
  {
    int num = 0;
    int *values;
    
    for (IntVarValues i(vars[id]); i(); ++i) 
      num++;

    values = (int*) malloc ((num+1) * sizeof(int));
    values[0] = num;
    num = 1;
    for (IntVarValues i(vars[id]); i(); ++i)
      values[num++] = i.val();
    
    return values;
  }
};
