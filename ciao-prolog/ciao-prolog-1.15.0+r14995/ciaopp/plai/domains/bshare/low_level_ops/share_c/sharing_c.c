/////////////////////////////////////////////////////////////////////////
// AUTHOR: Jorge Navas
// DESCRIPTION: This C file implements all operations required by the
// Set-Sharing abstract domain defined by Jacobs and Langen 1990:
// * abstract unification (amgu)
// * projection
// * equivalent
// * augment 
//  and all machinery needed to communicate with Ciao-Prolog.
/////////////////////////////////////////////////////////////////////////

#include <stdio.h>
#include <string.h>
#include <malloc.h>
#include <stdlib.h>
#include <string.h>

typedef int BOOL;

#define FALSE 0
#define TRUE 1
#define WORD_LENGTH 16
#define debug FALSE

/* turn a numeric literal into a hex constant
(avoids problems with leading zeroes)
8-bit constants max value 0x11111111, always fits in unsigned long
*/

#define HEX__(n) 0x##n##LU

/* 8-bit conversion function */
#define B8__(x) ((x&0x0000000FLU)?1:0) \
+((x&0x000000F0LU)?2:0) \
+((x&0x00000F00LU)?4:0) \
+((x&0x0000F000LU)?8:0) \
+((x&0x000F0000LU)?16:0) \
+((x&0x00F00000LU)?32:0) \
+((x&0x0F000000LU)?64:0) \
+((x&0xF0000000LU)?128:0)

/* for upto 8-bit binary constants */
#define B8(d) ((unsigned char)B8__(HEX__(d)))

/* for upto 16-bit binary constants, MSB first */
#define B16(dmsb,dlsb) (((unsigned short)B8(dmsb)<< \
+ B8(dlsb))

/* for upto 32-bit binary constants, MSB first */
#define B32(dmsb,db2,db3,dlsb) (((unsigned long)B8(dmsb)<<24) \
+ ((unsigned long)B8(db2)<<16) \
+ ((unsigned long)B8(db3)<<8) \
+ B8(dlsb))

/* Sample usage:
B8(01010101) = 85
B16(10101010,01010101) = 43605
B32(10000000,11111111,10101010,01010101) = 2164238933
*/

/* //////////////////////////////////////////////////////////////////////////// */
typedef unsigned int elem;

char * dec2bin(long decimal)
{
// accepts a decimal positive integer and returns a binary coded string

  int k = 0;
  int n = 0;
  int remain;
  // int old_decimal; // for test
  //char temp[WORD_LENGTH];
  char *binary = (char *)malloc(WORD_LENGTH+1);
  char *temp   = (char *)malloc(WORD_LENGTH+1);
  
  do
    {
      // old_decimal = decimal; // for test
      remain = decimal % 2;
      // whittle down the decimal number
      decimal = decimal / 2;
      // this is a test to show the action
      //printf("%d/2 = %d remainder = %d\n", old_decimal, decimal, remain);
      // converts digit 0 or 1 to character '0' or '1'
      if (k > WORD_LENGTH)
	printf("ERROR: the length of the strings must be increased\n");

      temp[k] = remain + '0';
      k++;
    } while (decimal > 0);
    
  int last = k;
  // reverse the spelling
  while (k >= 0){    
    k--;
    binary[n] = temp[k];
    n++;
  }
  binary[last] = '\0'; // end with NULL  
  //printf("Converting into string %s (length: %d) \n",binary,strlen(binary));

  return binary;
}


char * paddle_string(char * record,int N){
  // Paddle the string record with N-length(record) 0's to the left

  int length = strlen(record);
  int diff = N - length;
  //  char paddle[WORD_LENGTH];
  char * paddle = (char *)malloc(WORD_LENGTH+1);
  int i;

  // debugging
  //printf("String to be paddled ...%s\n",record);
  //printf("Its length ...%d\n",length);
  //printf("Number of 0's to insert .... %d\n",diff);
  if (diff < 0){
    printf("ERROR: length of string greater than expected\n");
  }
  else{

    if (diff == 0)
      strcpy(paddle,record);
    else{
      for(i=0;i < diff;i++){
	paddle[i] = '0';
      }
      // end with NULL
      paddle[i] = '\0';
      (void) strncat(paddle,record,strlen(record));
    }
    //    char * s_paddle;
    //(void) strcpy(s_paddle,paddle);
    //return s_paddle;
    return paddle;
  }

}

void elem_print(elem r,int N){

  //  char elem_string[WORD_LENGTH];
  char * elem_string;
  char * res_string;
/*   if (debug){ */
/*     printf("Printing %d (max length: %d) \n",r,N); */
/*   } */
  elem_string = dec2bin((long) r);
  res_string = paddle_string(elem_string,N);
  printf("%s",res_string);
  return;
}

void elem_fprint(FILE * fp,elem x,int N){

  //  char elem_string[WORD_LENGTH];
  char * elem_string;
  char * res_string;

  //  printf("Writing elem %d\n",x); ///
  elem_string = dec2bin((long) x);
  //printf("Converting into binary %s\n",elem_string); ///
  res_string = paddle_string(elem_string,N);
  //printf("Paddling 0's %s\n",res_string); ///
  fprintf(fp,"%s\n",res_string);

  return;
}

int elem_equal(elem x, elem y){
  if (x == y) return TRUE;
  else return FALSE;
}

int elem_less(elem x, elem y){
  if (x < y) return TRUE;
  else return FALSE;
}
////////////////////////////////////////////////////////////////////////////
typedef struct{
  elem first;
  struct LINKED_LIST *next;
} LINKED_LIST;

typedef struct{
  int length_vars;    // length of a string
  int card;           // cardinality of the set
  LINKED_LIST *list;  // elements of the set (sorted)
} SET;
////////////////////////////////////////////////////////////////////////////


SET * set_init(){
  
  SET * s = (SET *) malloc(sizeof(SET));
  s->card = 0;
  s->length_vars = 0;
  s->list = NULL;
  return s;
}

SET * set_copy(SET * s){

  SET * s_copy;
  if ((s == NULL) || (s->card == 0)){
    s_copy = (SET *) malloc(sizeof(SET));
    s_copy->card = 0;
    s_copy->length_vars = s->length_vars;
    s_copy->list = NULL;
  }
  else{
    LINKED_LIST * list_copy = (LINKED_LIST *) malloc(sizeof(LINKED_LIST));
    LINKED_LIST * current = s->list;
    list_copy->first = current->first;
    list_copy->next = NULL;
    current = (LINKED_LIST *) current->next;
    LINKED_LIST * current_copy = list_copy;

    while (current != NULL){
      current_copy->next = (struct LINKED_LIST *) malloc(sizeof(LINKED_LIST));
      current_copy = (LINKED_LIST *) current_copy->next;
      current_copy->first = current->first;
      current_copy->next = NULL;
      current = (LINKED_LIST *) current->next;
    }
    s_copy = (SET *) malloc(sizeof(SET));
    s_copy->card = s->card;
    s_copy->length_vars = s->length_vars;
    s_copy->list = list_copy;
  }
    return s_copy;
}

SET * set_add(SET * s, elem x, int N){
  // PRE:         s.list is a sorted list
  // DESCRIPTION: inserts the element x into s.list
  // POST:        s.list is a sorted set

  if (debug)
    printf("BEGIN set_add \n");
  SET * res = (SET *) malloc(sizeof(SET));
  int inserted = FALSE;

  if (s->card == 0){
    // insert first element because it was empty
    res->card = 1;
    res->length_vars = N;
    res->list  = (LINKED_LIST *) malloc(sizeof(LINKED_LIST));
    res->list->first = x;
    res->list->next  = NULL;
    if (debug)
      printf("Inserted %d as first element because it was empty\n",x);
  }
  else{
    res = set_copy(s);
    LINKED_LIST * current = res->list;
    if (elem_less(x,current->first)){
      // insert first position
      res->card++;
      if (N != res->length_vars)
	printf("%s\n","ERROR: adding a string with different length");
      LINKED_LIST* new = (LINKED_LIST *) malloc(sizeof(LINKED_LIST));
      new->first = x;
      new->next = (struct LINKED_LIST*) res->list;
      res->list = new;
      inserted = TRUE;
      if (debug)
	printf("Inserted %d as first element but it was not empty\n",x);

    }
    else{
      LINKED_LIST * next = (LINKED_LIST *) current->next;
      while ((next != NULL) && (!inserted)){
	if ( (elem_equal(x,next->first)) || (elem_equal(current->first,x)))
	  inserted = TRUE;
	else{
	  if (elem_less(x,next->first))  {
	    // insert intermediate position
	    LINKED_LIST* new = (LINKED_LIST *) malloc(sizeof(LINKED_LIST));
	    inserted = TRUE;
	    new->first = x;
	    new->next = (struct LINKED_LIST*) next;
	    current->next = (struct LINKED_LIST*) new;
	    res->card++;
	    if (N != res->length_vars)
	      printf("%s\n","ERROR: adding a string with different length");
	    if (debug)
	      printf("Inserted %d as intermediate element\n",x);
	  }
	  current = next ;
	  next = (LINKED_LIST *) next->next;
	}
      }
	if ( (!inserted) && (!elem_equal(x,current->first))){  // insert last position
	  LINKED_LIST* new = (LINKED_LIST *) malloc(sizeof(LINKED_LIST));
	  new->first = x;
	  new->next = NULL;
	  current->next = (struct LINKED_LIST *) new;
	  res->card++;
	  if (N != res->length_vars)
	    printf("%s\n","ERROR: adding a string with different length");
	  if (debug)
	    printf("Inserted %d as last element\n",x);
	
      }
    }
  }
  if (debug)
    printf("END set_add \n");
  return res;
}

void set_print(SET * s){

  if (s->card == 0){
    printf("S[%d,%d] = NULL\n",s->card,s->length_vars);
  }
  else{
    printf("S[%d,%d]={", s->card,s->length_vars);
    LINKED_LIST *p = s->list;
    while (p != NULL){
      elem_print(p->first,s->length_vars);
      if (p-> next != NULL)
	printf(",");
      p = (LINKED_LIST *) p->next;
    }
    printf("}\n");
  }
  return;
}

SET * set_remove_all(SET *s){
  
   LINKED_LIST * current = s->list;
   LINKED_LIST * next;
   while (current != NULL) {
      next = (LINKED_LIST *) current->next;
      free(current);
      s->card--;
      current = next;
   }
   free(s);
   return NULL;
   
}


BOOL set_compare(SET * s1, SET * s2){

  if (s1->length_vars != s2->length_vars){
    printf("ERROR (compare): length of strings are different\n");
    return -1;
  }

  if ( s1->card != s2->card){
    return FALSE;
  }
  else{
    int equal = TRUE;
    LINKED_LIST * p_s1 = s1->list;
    LINKED_LIST * p_s2 = s2->list;
    while ((p_s1 != NULL) && (p_s2 != NULL) && (equal)){
      if (elem_equal(p_s1->first,p_s2->first)){
	p_s1 = (LINKED_LIST *) p_s1->next;
	p_s2 = (LINKED_LIST *) p_s2->next;
      }
      else
	equal = FALSE;
    }
    return equal;
  }
}


SET * set_project(SET * s, elem x){
  
  SET * proj = set_init();
  if (s->card == 0){
    proj = set_copy(s);
    return proj;
  }
  else{
    int N = s->length_vars;
    LINKED_LIST * p_s = s->list;
    elem projection;

    while (p_s != NULL){
      projection = (p_s->first & x);
      if (projection != 0)  // empty set
	proj = set_add(proj,projection,N);
      p_s = (LINKED_LIST *) p_s->next;
    }
    return proj;
  }
}

///////////////////// AMGU ///////////////////////////

void set_split_lists(SET * s, elem x,SET ** inter, SET ** disj){

  SET * int_  =  set_init();
  SET * disj_ =  set_init();

  if (s->card == 0){
    int_  = set_copy(s);
    disj_ = set_copy(s);
  }
  else{
    int N = s->length_vars;
    
    LINKED_LIST * p_s = s->list;
    elem projected;

    while (p_s != NULL){
      projected = (p_s->first & x);
      if (projected != 0) // intersection
	int_  = set_add(int_,p_s->first,N);
      else                // difference
	disj_ = set_add(disj_,p_s->first,N);
      p_s = (LINKED_LIST *) p_s->next;
    }
  }
/*   if (debug){ */
/*     printf("BEGIN Split_lists:\n"); */
/*     set_print(int_); */
/*     set_print(disj_); */
/*     printf("END Split_lists:\n"); */
/*   } */
  (*inter) = int_;
  (*disj) = disj_;
  
  return;
}


SET * set_union(SET * s1, SET * s2){
  // PRE: \forall b1 \in s1, \forall b2 \in s2. |b1| = |b2|
  // It's equivalent to a set_add_all

  if (s1->card == 0)
    return set_copy(s2);
  else{
    if (s2->card == 0)
      return set_copy(s1);
    else{
      
      if (s1->length_vars != s2->length_vars)
	printf("ERROR: union of two sets with different record length\n");
      else{
	int N = s1->length_vars;
	
	if ( s1->card == 0)
	  return set_copy(s2);
	else{
	  if ( s2->card == 0)
	    return set_copy(s1);
	  else{
	    SET * s_union = set_copy(s1);
	    LINKED_LIST * p_s2  =    s2->list;
	    while (p_s2 != NULL){
	      s_union = set_add(s_union,p_s2->first,N);
	      p_s2 = (LINKED_LIST *) p_s2->next;
	    }
	    return s_union;
	  }
	}
      }
    }
  }
}
    

SET * set_binary_union(SET * s1, SET * s2){

  if (s1->card == 0)
    return set_init();
  else{
    if (s2->card == 0)
      return set_init();
    else{

      if (s1->length_vars != s2->length_vars){
	printf("ERROR: binary union of two sets with different record length\n");
	return NULL;
      }
      else{
	SET * res = set_init();
	res->length_vars = s1->length_vars;
	if ((s1->card == 0) || (s2->card == 0)){
	  return res;
	}
	else{
	  int N = s1->length_vars;
	  LINKED_LIST * p_s1 = s1->list;
	  LINKED_LIST * p_s2;
	  elem e1,e2,e;
	  while (p_s1 != NULL){
	    e1 = p_s1->first;
	    p_s2 = s2->list;
	    while (p_s2 != NULL){
	      e2 = p_s2->first;
	      e = e1 | e2;  // |: OR
	      res = set_add(res,e,N);
	      p_s2 = (LINKED_LIST *) p_s2->next;
	    }
	    p_s1 = (LINKED_LIST *) p_s1->next;
	  }
	  return res;
	}
      }
    }
  }
}
    
SET * add_to_star(SET * set,elem xs,int N){
  SET * star = set;
  elem ys,zs;
  LINKED_LIST * p = set->list;
  if (debug)
    printf("BEGIN add_to_star \n");
  while (p != NULL){
    ys = p->first;
    zs = ys | xs;
    star = set_add(star,ys,N);
    star = set_add(star,zs,N);
    p = (LINKED_LIST *) p->next;
  }

   star =  set_add(star,xs,N);
   if (debug)
     printf("END add_to_star \n");
   return star;
}

SET * set_star_union(SET * set){

  SET * star;
  int N = set->length_vars;
  star = set_init();
  star->length_vars = N;

  if (debug)
    printf("BEGIN set_star_union \n");
  if (set->card == 0)
    return star;
  else{
    LINKED_LIST * p = set->list;
    while (p != NULL){
      star = add_to_star(star,p->first,N);
      p = (LINKED_LIST *) p->next;
    }
  }
  if (debug)
    printf("END set_star_union \n");
  return star;
}

SET * set_amgu(SET * s, elem x, elem t){

  SET * disj = set_init();
  SET * inter = set_init();
  SET * disj_ = set_init();
  
  if (x == 0){
    printf("%s\n","ERROR (amgu): the variable x cannot be empty");
    return set_init();  // return NULL;
  }
  else{
    if (t == 0){
      // Make x ground (i.e. remove all sharing groups in which x appears)
      if (debug)
	printf("T is empty, then making X ground...\n");
      set_split_lists(s,x,&inter,&disj);
      return disj;
    }
    else{
      int N = s->length_vars;
      elem xt       = x | t;
      SET * sh_x    = set_init();
      SET * sh_t_   = set_init();
      SET * sh_bin  = set_init();
      SET * sh_star = set_init();
      SET * sh_amgu = set_init();
      if (debug){
	set_print(s);
	printf("x\n: ");
	elem_print(x,N);
	printf("\n t\n: ");
	elem_print(t,N);
      }
      set_split_lists(s,xt,&inter,&disj);
      if (debug){
	printf("\n x U vars(t):\n");
	elem_print(xt,N);
	printf("\n notSh_xt:\n");
	set_print(disj);
      }
      set_split_lists(s,x,&sh_x,&disj_);
      if (debug){
	printf("Sh_x:");
	set_print(sh_x);
      }
      set_split_lists(s,t,&sh_t_,&disj_);
      if (debug){
	printf("Sh_t:");
	set_print(sh_t_);
      }
      printf("BEGIN binary union\n");
      sh_bin    = set_binary_union(sh_x,sh_t_);
      if (debug){
	printf("Sh_x bin Sh_t:");
	set_print(sh_bin);
      }
      printf("END binary union\n");
      printf("BEGIN star\n");
      sh_star   = set_star_union(sh_bin);
      if (debug){
	printf("(Sh_x bin Sh_t)*:");
	set_print(sh_star);
      }
      printf("END star\n");
      sh_amgu = set_union(disj,sh_star);
      if (debug){
	printf("amgu:");
	set_print(sh_amgu);
      }
      
      set_remove_all(disj);
      set_remove_all(disj_);
      set_remove_all(inter);
      set_remove_all(sh_x);
      set_remove_all(sh_t_);
      set_remove_all(sh_bin);
      set_remove_all(sh_star);
      
      return sh_amgu;
    }
  }
}

///////////////////////////////////////////////////////////////////////
///                  INTERFACE C AND CIAO-PROLOG
///////////////////////////////////////////////////////////////////////

char * set_to_string(SET * set){

  // This function converts a sharing set into a char * which is natively
  // understood by the interface Ciao and C.

  int N = set->length_vars;
  //  char word[(N +1) * set->card];
  char * word;

  // Writing label
  word = dec2bin((long) N);
  char * p_label = word + strlen(word);
  *p_label++     = '#';
  *p_label = '\0';
  if (debug){
    printf("Ciao to prolog\n");
    printf("Word: %s\n",word);
  }
  //char elem_string[N+1];
  char * elem_string;
  char * next_word;
  LINKED_LIST * p_set  = set->list;
  while (p_set != NULL){
    elem_string = dec2bin((long) p_set->first);
    next_word = paddle_string(elem_string,N);
    if (debug)
      printf("Next word %s \n",next_word);
    strncat(word,next_word,strlen(next_word));
    if (debug)
      printf("Appending ... %s \n",word);
    p_set = (LINKED_LIST *) p_set->next;
  }
  // ending with NULL
  //  strncat(word,"\0",sizeof("\0"));
  if (debug){
    printf("Length of the coded set is %d\n",strlen(word));
    printf("Word: %s\n",word);
  }
  char * s_word;
  (void) strcpy(s_word,word);
  return s_word;
  //  return word;
}

int convert(char *buf, int cbase)
{
    int j;
    int bin;
    
    j = 0;
    bin = 0;                 // start the binary value at 0
    while (buf[j])
    {
        buf[j] -= 48;           // convert character for binary
        if (buf[j] > 16)
            buf[j] -= 7;        // character was probably letter
        if (buf[j] >= cbase)
            buf[j] -= 32;       // character was probably lower
        if (buf[j] >= cbase || buf[j] < 0)
            break;              // invalid character, done
        bin *= cbase;           // multiply by the base
        bin += buf[j];          // add current value
        j++;                    // next character
    }
    return bin;             // return the binary value
}

SET * string_to_set(char * s){
  // This function converts a string into a sharing set

  // Reading label

  int found = FALSE;
  //char label_s[WORD_LENGTH];
  char * label_s = (char *) malloc(WORD_LENGTH+1);
  int i = 0;

  char * s_copy = (char *) malloc(strlen(s)+1);
  (void) strcpy(s_copy,s);
  char * p_s = s_copy;

  int total_length = strlen(s_copy);
  if (debug){
    printf("Prolog to C\n");
    printf("Length of the coded set is %d\n",total_length);
  }

  while ( (p_s != NULL) && (!found)){
    if (*p_s != '#'){
      label_s[i] = *p_s++;
      i++;
    }
    else
      found = TRUE;
  }

  label_s[i] = '\0';
  if (debug)
    printf("Label %s\n",label_s);
  int N = convert(label_s,2);
  if (debug)
    printf("Length of variables is %d\n",N);


  // Reading rest
  if (debug)
    printf("Total length is %d\n",total_length);
  int processed = total_length - (i + 1);

  p_s++;
  if (debug)
    printf("Rest of string to process (old): %s\n",p_s);

  char * rest = (char *) malloc (processed+1);
  (void) strcpy(rest,p_s);

  if (debug){
    printf("Rest of string to process %s\n",rest);
    printf("Characters to process %d\n",processed);
  }

  SET * sh = set_init();
  i =0;
  char * word = (char *) malloc(N + 1);
  while (processed != 0){
    if (debug)
      printf("value of processed %d\n",processed);
    int j = 0;
    while (j < N){
      word[j] = rest[i];
      j++;
      i++;
    }
    word[j] = '\0';
    if (debug){
      printf("Word read %s\n",word);
      printf("Converted to integer %d\n",convert(word,2));
    }
    sh = set_add(sh,convert(word,2),N);
    processed = processed - N;
  }
  return sh;
}

/* char * ciao_sharing_amgu(char * s, int  x, int t){ */

/*   SET * sh         = string_to_set(s); */
/*   sh               = set_amgu(sh,(elem) x,(elem) t); */
/*   char * sh_string = set_to_string(sh); */
/*   return sh_string; */
/* } */


SET * read_file(char * fname){
  FILE* fp;
  if (debug)
    printf("BEGIN Reading a file\n");
  if ((fp = fopen(fname,"r")) == NULL){
    printf("ERROR (read_file): File could not be opened\n");
    return NULL;
  }
  else{
    // char word[WORD_LENGTH];
    char * word = (char *) malloc(WORD_LENGTH+1);
    int N;
    elem bits;
    SET * sh = set_init() ;
    // Read Label ///////////////////////////////
    // char label[WORD_LENGTH];
    char *label = (char *) malloc(WORD_LENGTH+1);
    if (fscanf(fp,"%s",label) < 0)
      printf("ERROR (read_file): reading label from file\n");
    else{
      label[0] = '0';  //eliminate 'b'
      N = atoi(label);
      if (debug)
	printf("Label %s\n",label);
    }
    /////////////////////////////////////////////
    while ( fscanf(fp,"%s",word) != EOF){
      if (debug)
	printf("Read the string %s\n",word);
      bits = convert(word,2);
      if (debug)
	printf("Converted into the number %d\n",bits);
      sh = set_add(sh,bits,N);
    }
    fclose(fp);
    if (debug)
      printf("END read file\n");
    return sh;
  }

}

void write_file(char * fname, SET * set){
  FILE* fp;
  if ((fp = fopen(fname,"w")) == NULL){
    printf("ERROR (write_file): file could not be opened\n");
  }
  else{
    // Write label //////////////////////////
    int N = set->length_vars;
    // char label_n[WORD_LENGTH];
    // char label[WORD_LENGTH];
    char * label_n = (char *) malloc(WORD_LENGTH+1);
    char *label = (char *) malloc(WORD_LENGTH+1);
    int i;
    if (debug)
      printf("Writing a file\n");
    if ((i= sprintf(label_n,"%d",N)) < 0)
      printf("ERROR (write_file): converting an integer into a string\n");
    else{
      label[0] = 'b';
      label[1] = '\0';
      (void) strncat(label,label_n,strlen(label_n));
      fprintf(fp,"%s\n",label);
      if (debug)
	printf("Label: %s \n",label);
    }
    /////////////////////////////////////////
    LINKED_LIST* p = set->list;
    while (p != NULL){
      if (debug){
	printf("Writing into a file %d\n",p->first);
      }
      elem_fprint(fp,p->first,N);
      p = (LINKED_LIST *) p->next;
    }
    if (debug)
	printf("END Writing a file\n");

    fclose(fp);
  }
  return;
}

void sharing_amgu_prolog(char* fname, char * x, char * t){
  // x is a string coded in binary
  // t is a string coded in binary
  SET * sh;
  if ((sh = read_file(fname)) != NULL){
    elem x_num = convert(x,2);
    elem t_num = convert(t,2);
    if (debug){
      printf("Unifying %d and %d given the sharing \n",x_num,t_num);
      set_print(sh);
    }
      
    sh = set_amgu(sh,x_num,t_num);
    write_file(fname,sh);
  }
  else
    printf("ERROR (sharing_amgu_prolog): File could not be opened\n");
  if (debug)
    printf("END amgu\n");
  return;
}
