#include <fcntl.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <sys/types.h>
#include <termios.h>
#include <unistd.h>
#include <pthread.h>

// #define READ_MANUALLY

volatile int compass_value = 0;
pthread_mutex_t mutex_compass = PTHREAD_MUTEX_INITIALIZER;

#define PI 3.1415926535897932384626433832795

#define TRACE(H) // fprintf(stderr, "In %s\n", H)

typedef struct {
	char *buf ;
	int r, w ;
	int max ;
} mystring ;


mystring* string_new( int m );
void string_reset(mystring *s);

typedef struct {
  unsigned short g1_min, g1_max;
  unsigned short g2_min, g2_max;
  
  short l1_min, l1_max;
  short l2_min, l2_max;
  short l3_min, l3_max;

} SensorRanges;

typedef struct {
  unsigned short g1_off, g1_scl;
  unsigned short g2_off, g2_scl;
  
  short l1_off, l1_scl;
  short l2_off, l2_scl;
  short l3_off, l3_scl;

} CalibrationData;

typedef struct {
  double ax;
  double ay;

  double r;
  
  double pitch;
  double roll;
  
  double h2d;
  double h3d;

} CompassReading;


typedef struct {
  SensorRanges range;
  CalibrationData calb;
  CompassReading data;

  int record_range_data;
  int new_reading;
  char id;
} Compass3D;



enum {MSG_INCOMPLETE, MSG_HANDLED, MSG_UNMATCHED};

void compass3d_send_poll(int fd);
void readpart(mystring *s, int fd);
int handle_compass3d_reading(mystring *s, Compass3D *compass3d);
void parse_compass3d_reading(char *p, Compass3D *compass3d);
double calcHeading3D(double ax, double ay, double vx, double vy, double vz);
void calc_calb_from_ranges(Compass3D *compass3d);
void init_compass3d_data(Compass3D *compass3d);
void default_ranges(Compass3D *compass3d);


mystring* string_new( int m ) {
  mystring *s = (mystring*) malloc( sizeof(mystring) ) ;
  s->r = 0 ;
  s->w = 0 ;
  s->max = m ;
  s->buf = (char*)malloc( 2*m*sizeof(char) ) ;
  return s ;
}


void string_reset(mystring *s) {
  s->r = s->w = 0;
}

void readpart(mystring *s, int fd) {
  int n, i ;

  TRACE("readpart");

  n = s->max - s->w ;
  if (s->w < s->r) 
    n = s->r - s->w ;

  // It might be the case that we cannot read all we want!  But we do not
  // delay here (because the fd was open with a non-blocking option).  We
  // just store what we can, and we go on.
  i = read(fd, s->buf + s->w, n) ;

  if( i < 0 ) perror( "Read in readpart()" );

#if defined(DEBUG)
  if (i != n) fprintf(stderr, "Requested %d, read %d\n", n, i);
#endif

  s->w += i ;
  if( s->w >= s->max ) s->w = 0;

  if (s->w < s->r) memcpy(s->buf + s->max, s->buf, s->w);
}

void init_compass3d_data(Compass3D *compass3d) {

  TRACE("init_compass3d_data");

  default_ranges(compass3d);
  calc_calb_from_ranges(compass3d);

  compass3d->id = '0';
  compass3d->record_range_data = 0;
  compass3d->new_reading = 0;

}

void default_ranges(Compass3D *compass3d) {
  SensorRanges *range = &(compass3d->range);

  TRACE("default_ranges");

  range->g1_min = 302;  range->g1_max = 705;
  range->g2_min = 301;  range->g2_max = 707;
  range->l1_min = 2780;  range->l1_max = 8120;
  range->l2_min = -2766;  range->l2_max = 2370;
  range->l3_min = -2500;  range->l3_max = 2750;
}


char poll_message[] = "$SC0P";

void compass3d_send_poll(int fd) {
  write(fd, poll_message, 5);
}

int handle_compass3d_reading(mystring *s, Compass3D *compass3d) {

  int n;
  char *x;

  TRACE("handle_compass3d_reading");

  // Perhaps mystring works as a buffer: Write part and Read part (w, r).
  // This would help to decouple read and write speeds.

  // Writing goes ahead of reading -- enough data?  Note that the buffer
  // size is *twice* the max -- hence no need to wrap around even if
  // "logical" buffer overflow.
  n = (s->w >= s->r) ? s->w - s->r : s->max - s->r + s->w;

  // We need 16 chars to complete a reading.
  if (n < 16) {
    TRACE("INCOMPLETE handle_compass3d_reading");
    return MSG_INCOMPLETE;
  }

  // Point to where our data starts
  x = s->buf + s->r;
  if (!strncmp(x, "$RC", 3) &&
      x[3] == compass3d->id &&
      x[4] == 'R' && x[15] == 0x0d) { // Sanity checks passed

    TRACE("CALLING parse_compass3d_reading");

    parse_compass3d_reading(x, compass3d);

    // Move reading pointer to next sample
    s->r += 16;
    // If we're over end of buffer, wrap around
    if (s->r >= s->max) s->r -= s->max; 
    return MSG_HANDLED; // Not used?
  } else fprintf(stderr, "Compass reading wrong: x[3] = %c\n", x[3]);
  return MSG_UNMATCHED;
}

void calc_calb_from_ranges(Compass3D *compass3d) {

  SensorRanges *range = &(compass3d->range);
  CalibrationData *calb = &(compass3d->calb);

  TRACE("calc_calb_from_ranges");

  calb->g1_off = (range->g1_min + range->g1_max)/2;
  calb->g1_scl = (range->g1_max - range->g1_min)/2;

  calb->g2_off = (range->g2_min + range->g2_max)/2;
  calb->g2_scl = (range->g2_max - range->g2_min)/2;

  calb->l1_off = (range->l1_min + range->l1_max)/2;
  calb->l1_scl = (range->l1_max - range->l1_min)/2;

  calb->l2_off = (range->l2_min + range->l2_max)/2;
  calb->l2_scl = (range->l2_max - range->l2_min)/2;

  calb->l3_off = (range->l3_min + range->l3_max)/2;
  calb->l3_scl = (range->l3_max - range->l3_min)/2;

}



void parse_compass3d_reading(char *p, Compass3D *compass3d) {

  CalibrationData *calb = &(compass3d->calb);

  unsigned char *s = (unsigned char*)p;
  unsigned short g1, g2;
  short	l1, l2, l3;
  double ax, ay, mx, my, mz;

  TRACE("parse_compass3d_reading");

  compass3d->id = p[3];  // Always '0' (and it remains unchanged...)

  // Use the rest of the string as 16 bit numbers
  g1 = (((unsigned short)*(s+5)) << 8) | *(s+6);
  g2 = (((unsigned short)*(s+7)) << 8) | *(s+8);

  ax = -((double)g2 - calb->g2_off)/calb->g2_scl;
  ay =  ((double)g1 - calb->g2_off)/calb->g1_scl;

  if (ax > 1.0) ax = 1.0;
  if (ax < -1.0) ax = -1.0;
  if (ay > 1.0) ay = 1.0;
  if (ay < -1.0) ay = -1.0;

  compass3d->data.ax = ax;
  compass3d->data.ay = ay;

  l1 = (short)(*(s+ 9) << 8) | *(s+10);
  l2 = (short)(*(s+11) << 8) | *(s+12);
  l3 = (short)(*(s+13) << 8) | *(s+14);

  mx = -((double)l3 - calb->l3_off)/calb->l3_scl;
  my = -((double)l2 - calb->l2_off)/calb->l2_scl;
  mz = -((double)l1 - calb->l1_off)/calb->l1_scl;

  compass3d->data.h3d = calcHeading3D(ax, ay, mx, my, mz);

  compass3d->new_reading = 1;
}


double calcHeading3D(double ax, double ay, double vx, double vy, double vz) {

  double mx, my, mz;
  double L = -atan2(ay, ax);
  double cosL = cos(L);
  double sinL = sin(L);
  double R;
  double cosR;
  double sinR;

  double d = sqrt(ax*ax + ay*ay);

  TRACE("calcHeading3D");

  if (d > 1.0) d = 1.0;

  R    = -asin(d);
  cosR = cos(R);
  sinR = sin(R);

  mx = ((cosL*cosL*cosR + sinL*sinL)*vx +
        (-cosL*cosR*sinL +sinL*cosL)*vy +
        (cosL*sinR)*vz);

  my = ((-sinL*cosR*cosL + cosL*sinL)*vx +
        (sinL*sinL*cosR + cosL*cosL)*vy +
        (-sinL*sinR)*vz);

  mz = (-sinR*cosL)*vx + (sinR*sinL)*vy + (cosR)*vz;

  return 180.0*atan2(my, mx)/PI;

}


int openGumstixBus(void);

int openGumstixBus(void) {
  int fd = open("/dev/ttyS3",O_RDWR);

  if( fd == -1 ) return -1;

  return fd;
}

void init_device() {
  struct termios tios ;
  int fd = open("/dev/ttyS3",O_RDWR);

  if( fd == -1 ) {
    perror("Init gumstix");
    exit(1);
  }

  tcgetattr( fd, &tios ) ;
  cfsetospeed( &tios, B9600 ) ;
  cfsetispeed( &tios, B9600 ) ;
  cfmakeraw( &tios ) ;
  tcsetattr( fd, TCSANOW, &tios ) ;

  close(fd);
}

#if defined(READ_MANUALLY)
void * read_loop(void *arg)
{
  while (1){
    printf("Compass? ");
    scanf("%d", &compass_value);
  }
}
#else
void *read_loop(void *arg) {
  Compass3D compass3d;
  int fd_bus, rv;
  struct timeval to;
  fd_set read_fds;

  mystring *input_from_compass = string_new(100);

  if (!(fd_bus = openGumstixBus())) {
    fprintf(stderr, "Couldn't open com port!\n");
    exit(1);
  }

  init_compass3d_data(&compass3d);

  while (1) {
    to.tv_sec = 0;
    // JF: it was 200000, I changed it because it was too fast if fprintf was disabled here
    to.tv_usec = 400000; // Trying to poll faster makes it fail?
    FD_ZERO(&read_fds);
    FD_SET(fd_bus, &read_fds);
    rv = select(fd_bus+1, &read_fds, NULL, NULL, &to);

    compass3d_send_poll(fd_bus);

    // Can we read from the compass?
    if (FD_ISSET(fd_bus, &read_fds)) {
  
      readpart(input_from_compass, fd_bus);   
    
      handle_compass3d_reading(input_from_compass, &compass3d);

      if (compass3d.new_reading) {
        pthread_mutex_lock(&mutex_compass);
	compass_value = (int)compass3d.data.h3d;
        pthread_mutex_unlock(&mutex_compass);
	//	fprintf(stdout, "%.2f.\n", compass3d.data.h3d);
	//        fflush(stdout);
	compass3d.new_reading = 0;
      }
    } // else fprintf(stderr, "Not ready\n");
    
    if (rv == 0)  /* timeout! */
      string_reset(input_from_compass);
  }
  return NULL;
}
#endif

pthread_t read_thread;
pthread_attr_t read_thread_attr;


void init_compass() {
#if !defined(READ_MANUALLY)
  init_device();
#endif

  pthread_attr_init(&read_thread_attr);
  pthread_attr_setdetachstate(&read_thread_attr, PTHREAD_CREATE_JOINABLE);
  pthread_attr_setscope(&read_thread_attr, PTHREAD_SCOPE_SYSTEM);

  pthread_create(&read_thread, &read_thread_attr, read_loop, NULL);

}



