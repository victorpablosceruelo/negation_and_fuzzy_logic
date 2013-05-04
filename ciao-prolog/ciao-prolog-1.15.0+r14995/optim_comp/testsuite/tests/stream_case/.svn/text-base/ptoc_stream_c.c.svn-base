#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include <sys/time.h>
#include <sys/resource.h>

int userclick() {
  struct rusage rusage;
  getrusage(RUSAGE_SELF,&rusage);
  return ((int)rusage.ru_utime.tv_sec) * 1000000 + rusage.ru_utime.tv_usec;
}

#define NEW(Type) ((Type *)malloc(sizeof(Type)))

// Naive version of the stream interpreter for ptoc
// (modified by Jose Morales <jfran@clip.dia.fi.upm.es>)
// (initial version by Manuel Carro <mcarro@fi.upm.es>)

// ---------------------------------------------------------------------------
// Parameters
#define AUDIO_SPS 4410
#define COMPASS_SPS 10

#define NUM_DATA 150000
#define EXTRA_DATA (NUM_DATA * 2)

// ---------------------------------------------------------------------------
// Data structures
typedef struct audio audio_t;
struct audio {
  double left;
  double right;
};

typedef struct gps gps_t;
struct gps {
  double left;
  double right;
};

typedef struct audiolist audiolist_t;
struct audiolist {
  audio_t head;
  audiolist_t *tail;
};

typedef struct gpslist gpslist_t;
struct gpslist {
  gps_t head;
  gpslist_t *tail;
};

typedef struct complist complist_t;
struct complist {
  double head;
  complist_t *tail;
};

// ---------------------------------------------------------------------------
// Process data (naive)

audio_t *nthaudio(int N, audiolist_t *List) {
  if (N == 1) {
    return &List->head;
  } else {
    return nthaudio(N - 1, List->tail);
  }
}  

void stream(int N,
	    complist_t *Comp,
	    gpslist_t *Gps,
	    audiolist_t *AudioIn,
	    audiolist_t **AudioOut) {
 again:
  if (N <= 0) {
    *AudioOut = NULL;
  } else {
    double Compass = Comp->head;
    double GLeft = Gps->head.left;
    double GRight = Gps->head.right;
    audio_t *aud = &AudioIn->head;
    double One = aud->left;
    *AudioOut = NEW(audiolist_t);
    // GPS data is constant for every second -> during AUDIO_SAMPLES_PER_SEC
    // the value of t will not change; therefore we could calculate T just
    // once per second (i.e., once every AUDIO_SAMPLES_PER_SEC samples).
    // Note that it involves a costly atan2 operation!
    double At = atan(GRight/GLeft);
    double T = 55 * At;
    // COMPASS data changes 10 times (i.e., COMPASS_SAMPLES_PER_SEC) every
    // second.  Therefore we need to calculate angle and offset only once
    // every 441 (for this case) audio samples.
    double Angle0 = fmod(floor(T+Compass), 360) - 180;
    int Angle = Angle0;
    double Offset = floor(fabs((double)Angle0/6.0));
    // Give a 3-D feeling by changing the phase of the sound received by
    // every ear.  Note: assuming the compass is centered in the head, this
    // should probably be 
    //    one = data[pos - offset / 2][AUDIO].left;
    //    other = data[pos + offset / 2 ][AUDIO].left;
    double RealOffset = Offset+2;
    audiolist_t *NewAud = NEW(audiolist_t);
    NewAud->head.left = aud->left;
    NewAud->head.right = aud->right;
    NewAud->tail = AudioIn->tail;
    audio_t *audio = nthaudio(RealOffset, NewAud);
    double Other = audio->left;
    // I guess that if the head has turned around, the 'left' sound source
    // goes into the right ear and the other way around.
    audio_t aout;
    if (Angle < 0) {
      aout.left = One;
      aout.right = Other;
    } else {
      aout.left = Other;
      aout.right = One;
    }
    (*AudioOut)->head.left = aout.left;
    (*AudioOut)->head.right = aout.right;
    //    printf("%d %d\n", (int)aout.left, (int)aout.right);
    int N1 = N - 1;
    // note: tail recursion
    N = N1;
    Comp = Comp->tail;
    Gps = Gps->tail;
    AudioIn = AudioIn->tail;
    AudioOut = &((*AudioOut)->tail);
    goto again;
  }
}

void dostream(int NSamples,
	      complist_t *Compass,
	      gpslist_t *Gps,
	      audiolist_t *AudioIn) {
  audiolist_t *AudioOut;
  stream(NSamples, Compass, Gps, AudioIn, &AudioOut);
}

void process_data(complist_t *Compass,
		  gpslist_t *Gps,
		  audiolist_t *AudioIn) {
  dostream(NUM_DATA, Compass, Gps, AudioIn);
}

// ---------------------------------------------------------------------------
// Gen data eager

void generate_data(int N, int M,
		   complist_t **Compass,
		   gpslist_t **Gps,
		   audiolist_t **AudioIn) {
 again:
  if (N == M) {
    *Compass = NULL;
    *Gps = NULL;
    *AudioIn = NULL;
  } else {
    *Compass = NEW(complist_t);
    double Au = AUDIO_SPS;
    double Com = COMPASS_SPS;
    double C = fmod((N / (Au / Com)), 360);
    (*Compass)->head = C;
    *Gps = NEW(gpslist_t);
    (*Gps)->head.left = 50;
    (*Gps)->head.right = 50;
    *AudioIn = NEW(audiolist_t);
    (*AudioIn)->head.left = N + 1;
    (*AudioIn)->head.right = 0;

    // note: tail recursion
    N = N + 1;
    Compass = &((*Compass)->tail);
    Gps = &((*Gps)->tail);
    AudioIn = &((*AudioIn)->tail);
    goto again;
  }
}

// ---------------------------------------------------------------------------

int main() {
  complist_t *Compass;
  gpslist_t *Gps;
  audiolist_t *AudioIn;
  int t1, t2;
  t1 = userclick();
  generate_data(0, EXTRA_DATA, &Compass, &Gps, &AudioIn);
  process_data(Compass, Gps, AudioIn);
  t2 = userclick();
  printf("%f\n", (t2-t1)/1000000.0);
  return 0;
}

