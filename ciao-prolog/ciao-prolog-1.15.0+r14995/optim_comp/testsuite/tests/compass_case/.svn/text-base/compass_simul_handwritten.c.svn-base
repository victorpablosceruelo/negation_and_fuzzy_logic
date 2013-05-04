#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define PARAM_FREQUENCY 44100
//#define PARAM_COMPASS 44100
#define PARAM_COMPASS 10
#define USE_FAST_ANGLE 1

// Compass programmed by hand
// (Jose Morales, based on previous code)

/***************************************************************************/
/* Sound and compass access */

/* To be fair, this mimic the prolog interface */
FILE *ids[32];
int ids_count = 0;
int out_id;

#define ONE_OUTPUT 1

typedef unsigned short sample_t;

int open_read_mono(FILE *f) {
  int ids_current;
  ids_current = ids_count;
  ids[ids_count] = f;
  ids_count++;
  return ids_current;
}

void open_write_stereo(FILE *f) {
  int ids_current;
  ids_current = ids_count;
  ids[ids_count] = f;
  ids_count++;

  out_id = ids_current;
}

sample_t read_mono(int in_id) {
  int r;
  unsigned short mono;
  FILE *f;
  
  f = ids[in_id];

  r = getc(f);
  if (r == -1) goto end_of_stream;
  int r2 = getc(f);
  if (r2 == -1) goto end_of_stream;
  mono = (r2 << 8) + r;
  return mono;
  
 end_of_stream:
  exit(-1);
}

void write_stereo(sample_t left, sample_t right) {
  int r;
  FILE *f;

  f = ids[out_id];

  r = putc(left & 255, f);
  if (r == -1) goto end_of_stream;
  r = putc(left >> 8, f);
  if (r == -1) goto end_of_stream;
  r = putc(right & 255, f);
  if (r == -1) goto end_of_stream;
  r = putc(right >> 8, f);
  if (r == -1) goto end_of_stream;

  return;
 end_of_stream:
  exit(-1);
}

int read_compass() {
  static int angle = 0;
#if defined(USE_FAST_ANGLE)
  angle++;
  return angle;
#else
  angle++;
  return angle >> 12;
#endif
}

#if !defined(PARAM_FREQUENCY)
#error "No PARAM_FREQUENCY. E.g. use -DPARAM_FREQUENCY=44100"
#endif

#if !defined(PARAM_COMPASS)
#error "No PARAM_COMPASS. E.g. use -DPARAM_COMPASS=10"
#endif

// This _has_ to match the actual sampling frequency
int audio_sps() { return PARAM_FREQUENCY; }

typedef struct _list list_t;
struct _list {
  int refcount;
  sample_t elem;
  list_t *rest;
};

void new_sample_cycle(int Cycle, int *APC, int InId,
		      int CurrSkip, int *NewSkip,
		      list_t **SL, list_t **SR, list_t ***NSL, list_t ***NSR);
list_t **skip_nth(int N, int InId, list_t **Samples);
void skip(int Disp, int InId,
	  list_t **L, list_t **R,
	  list_t ***NL, list_t ***NR);

void play_stereo(int Samples_Remaining, int InId, int CurrSkip);

int main(int argc, char **argv) {
  FILE *f;
  f = fopen(argv[1], "r");
  int InId;
  InId = open_read_mono(f);
  open_write_stereo(stdout);
  // Just for documentation: looking North
  // N.B.: we should calibrate to determine the initial position
  // of the sound source!
  play_stereo(1, InId, 0);
  return 0;
}

void new_sample(int InId, list_t **Samples) {
  if (*Samples == NULL) {
    // Advance (and instantiate, if needed) the input list.
    *Samples = (list_t *)malloc(sizeof(list_t));
    sample_t elem;
    elem = read_mono(InId);
    (*Samples)->refcount = 2;
    (*Samples)->elem = elem;
    (*Samples)->rest = NULL;
  }
}

// MCL: add a bogus fact?  To make analyzers happy, giving the program
// a possibility to scape cleanly.
void play_stereo(int Samples_Remaining, int InId, int CurrSkip) {
  // Skip the necessary number of samples according to the
  // orientation of the compass.
  list_t **NewSampleL;
  list_t **NewSampleR;
  int NewCycle;
  int NewSkip;
  sample_t R;
  list_t **RestSampleRight;
  sample_t L;
  list_t *Sample;
  list_t **RestSampleLeft;
  list_t **SampleL;
  list_t **SampleR;

  // Get one of them
  Sample = NULL;
  SampleL = &Sample;
  SampleR = &Sample;
 again:
  new_sample_cycle(Samples_Remaining, &NewCycle, InId,
		   CurrSkip, &NewSkip,
		   SampleL, SampleR, &NewSampleL, &NewSampleR);
  // Get two new samples for the right and left channels
  new_sample(InId, NewSampleR);
  R = (*NewSampleR)->elem;
  ((*NewSampleR)->refcount)--;
  RestSampleRight = &((*NewSampleR)->rest);

  new_sample(InId, NewSampleL);
  L = (*NewSampleL)->elem;
  ((*NewSampleL)->refcount)--;
  RestSampleLeft = &((*NewSampleL)->rest);

  // Play this sample and continue.
  write_stereo(R, L);

  // Free dead objects
  if (Sample != NULL) {
    while (Sample->rest != NULL && Sample->rest->refcount == 0) {
      list_t *old;
      old = Sample;
      Sample = Sample->rest;
      free(old);
    }
  }
  
  SampleL = RestSampleLeft;
  SampleR = RestSampleRight;
  CurrSkip = NewSkip;
  Samples_Remaining = NewCycle;
  goto again;
}

int compass_sps() { return PARAM_COMPASS; }

int audio_per_compass() {
  return audio_sps() / compass_sps();
}

int sound_speed() { return 340; }
double head_radius() { return 0.1; }
double pi() { return 3.141592; }

// How many samples are needed to fill in a meter of air.
double samples_per_meter() {
  return (double)audio_sps() / sound_speed();
}

// How many meters is some ear displaced North with respect to the
// center of the head.
double ear_dif(int Angle) {
  return head_radius() * sin((Angle * pi()) / 180);
}

// How many samples left and right ear differ, according to the angle
// the head has turned w.r.t. the North.
int find_skip(int Angle) {
  int SamplesDif;
  SamplesDif = (int)(samples_per_meter() * 2 * ear_dif(Angle));
  return SamplesDif;
}

// When we have read enough audio samples, read the compass and find
// out whether we have to skip or not samples from some channel.
void new_sample_cycle(int Cycle, int *APC, int InId,
		      int CurrSkip, int *NewSkip,
		      list_t **SL, list_t **SR, list_t ***NSL, list_t ***NSR) {
  if (Cycle == 1) {
    int Angle;
    int Diff;
    *APC = audio_per_compass();
    Angle = read_compass();
    *NewSkip = find_skip(Angle);
    Diff = (*NewSkip) - CurrSkip;
    skip(Diff, InId, SL, SR, NSL, NSR);
  } else {
    *APC = Cycle - 1;
    *NewSkip = CurrSkip;
    *NSL = SL;
    *NSR = SR;
  }
}

// This predicate is only called from time to time (e.g., only when
// we need read from the compass).  But we might want to read from the
// compass very often.
void skip(int Disp, int InId,
	  list_t **L, list_t **R,
	  list_t ***NL, list_t ***NR) {
  if (Disp == 0) {
    *NL = L;
    *NR = R;
  } else if (Disp > 0) {
    *NL = L;
    *NR = skip_nth(Disp, InId, R);
  } else {
    int D = -Disp;
    *NR = R;
    *NL = skip_nth(D, InId, L);
  }
}

list_t **skip_nth(int N, int InId, list_t **Samples) {
  if (N == 1) {
    new_sample(InId, Samples);
    ((*Samples)->refcount)--;
    return &((*Samples)->rest);
  } else {
    new_sample(InId, Samples);
    ((*Samples)->refcount)--;
    return skip_nth(N - 1, InId, &((*Samples)->rest));
  }
}

      


