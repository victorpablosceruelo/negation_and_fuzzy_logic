#include <engine/engine__definitions.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <strings.h>

// #define USE_UDP

FILE *ids[32];
int ids_count = 0;

CBOOL__PROTO(open_read_mono) {
  ERR__FUNCTOR("sound_access:open_read_mono", 2);
  stream_node_t *s;
  int ids_current;
  int i;
  
  s = stream_to_ptr_check(X(0), 'r', &i);
  if (!s) BUILTIN_ERROR(i,X(0),1);
  FILE *f = s->streamfile;
  ids_current = ids_count;
  ids[ids_count] = f;
  ids_count++;
  CBOOL__LASTUNIFY(X(1),MakeSmall(ids_current));
}

int newsockfd;
#if defined(USE_UDP)
struct sockaddr from;
struct sockaddr * __restrict__ fromp;
socklen_t fromlen;
#endif

CBOOL__PROTO(open_write_stereo) {
  //  ERR__FUNCTOR("sound_access:open_write_stereo", 1);
  
  int sockfd, portno;
  struct sockaddr_in serv_addr; 
#if !defined(USE_UDP)   
  struct sockaddr_in cli_addr;
  socklen_t clilen;
#endif

#if defined(USE_UDP)
  //  fprintf(stderr, "Opening socket\n");
  fromp = &from;
  sockfd = socket(AF_INET, SOCK_DGRAM, 0);
  //  fprintf(stderr, "Socket created\n");
#else
  sockfd = socket(AF_INET, SOCK_STREAM, 0);
#endif

  portno = 3003; // !!!!!!!!!!!!!!!!!!!!!!
  bzero((char *)&serv_addr, sizeof(serv_addr));
  serv_addr.sin_family = AF_INET;
  serv_addr.sin_addr.s_addr = INADDR_ANY;
  serv_addr.sin_port = htons(portno);
#if defined(USE_UDP)
  //  fprintf(stderr, "About to bound socket\n");
#endif
  bind(sockfd, (struct sockaddr*)&serv_addr, sizeof(serv_addr));

#if defined(USE_UDP)
  //  fprintf(stderr, "Socket bound\n");
  fromlen = sizeof(struct sockaddr_in);
  newsockfd = sockfd;
  // fprintf(stderr, "Received initial message\n");
#else
  listen(sockfd, 5);
  clilen = sizeof(cli_addr);
  newsockfd = accept(sockfd, (struct sockaddr *)&cli_addr, &clilen);
#endif
  CBOOL__PROCEED;
}

CBOOL__PROTO(close_write_stereo) {
  //  ERR__FUNCTOR("sound_access:open_write_stereo", 1);
  close(newsockfd);
  CBOOL__PROCEED;
}

CBOOL__PROTO(read_mono) {
  ERR__FUNCTOR("sound_access:read_mono", 2);

  int r;
  unsigned short mono;
  FILE *f;
  
  DEREF(X(0), X(0));
  CBOOL__TEST(TagIsSmall(X(0)));
  f = ids[GetSmall(X(0))];

  r = getc(f);
  if (r == -1) goto end_of_stream;
  int r2 = getc(f);
  if (r2 == -1) goto end_of_stream;
  mono = (r2 << 8) + r;
  CBOOL__LASTUNIFY(X(1),MakeSmall(mono));
  
 end_of_stream:
  BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, PAST_END_OF_STREAM),X(0),1)
}

#define BS 128
#define BUFFER_SIZE BS*4
unsigned char buffer[BUFFER_SIZE];
int bufidx = 0;

CBOOL__PROTO(write_stereo) {
  unsigned short left, right;
#if defined(USE_UDP)
  char message[4];
#endif

  DEREF(X(0), X(0));
  CBOOL__TEST(TagIsSmall(X(0)));
  left = GetSmall(X(0));

  DEREF(X(1), X(1));
  CBOOL__TEST(TagIsSmall(X(1)));
  right = GetSmall(X(1));

  buffer[bufidx++] = left >> 8;
  buffer[bufidx++] = left & 255;
  buffer[bufidx++] = right >> 8;
  buffer[bufidx++] = right & 255;

  if (bufidx == BUFFER_SIZE) {
#if defined(USE_UDP)
  // Wait for datagram -- all of them are going to come from the same
  // address, so we set it.
  if (recvfrom(newsockfd, message, 4, 0, fromp, &fromlen)<0)
    perror("recvfrom():");
  sendto(newsockfd, buffer, BUFFER_SIZE, 0, fromp, fromlen);
#else
  write(newsockfd, buffer, BUFFER_SIZE);
#endif
    bufidx = 0;
  }

  CBOOL__PROCEED;
}


CBOOL__PROTO(read_compass) {
#if defined(USE_REAL_COMPASS)
  int local_compass;
  pthread_mutex_lock(&mutex_compass);
  local_compass = compass_value;
  pthread_mutex_unlock(&mutex_compass);
  CBOOL__LASTUNIFY(X(0),MakeSmall(local_compass));
#else
  static int angle = 0;
  //#define USE_FAST_ANGLE 1
#if defined(USE_FAST_ANGLE)
  angle++;
  //  angle+=10;
  CBOOL__LASTUNIFY(X(0),MakeSmall(angle));
#else
  angle++;
  CBOOL__LASTUNIFY(X(0),MakeSmall(angle >> 12));
#endif
#endif
}

CBOOL__PROTO(init_compass_prolog) {
#if defined(USE_REAL_COMPASS)
  //fprintf(stderr, "Initializing compass\n");
  init_compass();
  //fprintf(stderr, "Done\n");
#endif
  CBOOL__PROCEED;
}
