#include <stdio.h>
#include <assert.h>

#define CHAR_T 8

void print_range(int startarg, int endarg, int binmodearg)
{
  int i = startarg;
  printf("print_range: ");
  while(i <= endarg) {
    if(!binmodearg){   /* convert range into 8 bits each */
      int j;
      int n = i<<3;
      for (j=0; j < CHAR_T; j++){
        printf("%d ",n+j);
      }
    } else {
      printf("%d ",i);
    }
    i++;
  }
  printf("\n");
}

int make_num(char * fmstart, int lenarg) {
  char * tostr;
  int rtnnum = -1;

  assert(lenarg > 0 && fmstart != NULL);

  tostr = malloc(sizeof(char) * (lenarg+1));
  printf("make-num: lenarg is %d, starting at %c\n",lenarg,fmstart[0]);
  memcpy(tostr,fmstart,lenarg+1);
  tostr[lenarg] = '\0';
  rtnnum = atoi((const) tostr);
  printf("make-num: returning %d from %s\n",rtnnum,tostr);
  free(tostr);
  return rtnnum;
}

int main(){
  int start, len;
  int listlen, i, num;
  int rangeflag = 0;
  int rangestart = -1;
  char * listarg = "2,5-7,10a";
  int binmode = 0;
  int plen = 96;  /* input as bits, converted if not binmode to letters */

  plen = (!binmode ? plen / CHAR_T : plen);

  listlen = strlen(listarg);
  printf("listlen is %d\n",listlen);
  i = 0;
  len = 0;
  start = 0;

  while(i < listlen){
    char c = listarg[i];
    printf("c at %d is %c\n",i,c);
    switch (c) {
    case ',':
      printf("at comma: start is %d, len is %d\n",start,len);
      num = make_num(&listarg[start],len);
      assert(num >= 0 && num < plen);
      len = 0;
      start = i+1;
      if(!rangeflag){
        rangestart = num;
      } else {
        rangeflag = 0;
      }
      printf("at comma: range from %d to %d\n",rangestart,num);
      print_range(rangestart,num,binmode);
      break;
    case '-':
      printf("at dash: start is %d, len is %d, char is %c\n",
             start,len,listarg[start]);
      rangestart = make_num(&listarg[start],len);
      assert(num >= 0 && num < plen);
      printf("at dash: rangestart is %d\n",rangestart);
      len = 0;
      start = i+1;
      rangeflag = 1;
      break;
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
      len++;
      break;
    case '\n':
    case ' ':
    default:   
      fprintf(stderr,"parse_list: invalid character [%c] at position %d\n",c,i+1);
      exit(-3);
    };
    i++;
  }
  /* last one */
  printf("at end: start is %d, len is %d\n",start,len);
  num = make_num(&listarg[start],len);
  if(!rangeflag){
    rangestart = num;
  }
  print_range(rangestart,num,binmode);

  printf("size of a long is %d bytes\n",sizeof(long));
  return 1;
}
