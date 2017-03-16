#include <math.h>
#include <stdio.h>
#include <stdlib.h>

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

// ---------------------------------------------------------------------------
// Process data (naive)

double atan2(double X, double Y) {
  // Needs to be changed -- atan is NOT atan2!!!
  return atan(X/Y);
}

void stream(int N, int M,
	    double *Comp,
	    gps_t *Gps,
	    audio_t *AudioIn,
	    audio_t *AudioOut) {
 again:
  if (N >= M) {
  } else {
    double Compass = Comp[0];
    double GLeft = Gps[0].left;
    double GRight = Gps[0].right;
    audio_t *aud = &AudioIn[0];
    double One = aud->left;
    // GPS data is constant for every second -> during AUDIO_SAMPLES_PER_SEC
    // the value of t will not change; therefore we could calculate T just
    // once per second (i.e., once every AUDIO_SAMPLES_PER_SEC samples).
    // Note that it involves a costly atan2 operation!
    double At = atan2(GRight, GLeft);
    double T = 55 * At;
    // COMPASS data changes 10 times (i.e., COMPASS_SAMPLES_PER_SEC) every
    // second.  Therefore we need to calculate angle and offset only once
    // every 441 (for this case) audio samples.
    double Angle = fmod(floor(T+Compass), 360) - 180;
    double Offset = floor(fabs((double)Angle/6.0));
    // Give a 3-D feeling by changing the phase of the sound received by
    // every ear.  Note: assuming the compass is centered in the head, this
    // should probably be 
    //    one = data[pos - offset / 2][AUDIO].left;
    //    other = data[pos + offset / 2 ][AUDIO].left;
    double RealOffset = Offset+2;
    int intoff = (int)RealOffset;
    audio_t *audio;
    if (intoff == 1) {
      audio = aud;
    } else {
      audio = &AudioIn[N - 1];
    }
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
    AudioOut[N].left = aout.left;
    AudioOut[N].right = aout.right;
    //    printf("%d %d\n", (int)aout.left, (int)aout.right);
    int N1 = N + 1;
    // note: tail recursion
    N = N1;
    goto again;
  }
}

audio_t AudioOut[NUM_DATA];

void dostream(int NSamples,
	      double *Compass,
	      gps_t *Gps,
	      audio_t *AudioIn) {
  stream(0, NSamples, Compass, Gps, AudioIn, AudioOut);
}

void process_data(double *Compass,
		  gps_t *Gps,
		  audio_t *AudioIn) {
  dostream(NUM_DATA, Compass, Gps, AudioIn);
}

// ---------------------------------------------------------------------------
// Gen data eager

void generate_data(int N, int M,
		   double *Compass,
		   gps_t *Gps,
		   audio_t *AudioIn) {
 again:
  if (N >= M) {
  } else {
    double Au = AUDIO_SPS;
    double Com = COMPASS_SPS;
    double C = fmod((N / (Au / Com)), 360);
    Compass[N] = C;
    Gps[N].left = 50;
    Gps[N].right = 50;
    AudioIn[N].left = N + 1;
    AudioIn[N].right = 0;

    // note: tail recursion
    N = N + 1;
    goto again;
  }
}

// ---------------------------------------------------------------------------

double Compass[EXTRA_DATA];
gps_t Gps[EXTRA_DATA];
audio_t AudioIn[EXTRA_DATA];

int main() {
  generate_data(0, EXTRA_DATA, Compass, Gps, AudioIn);
  process_data(Compass, Gps, AudioIn);
  return 0;
}

