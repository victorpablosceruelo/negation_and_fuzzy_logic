#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <linux/soundcard.h>
#include <math.h>
#include <netinet/in.h>
#include <netdb.h>
#include <strings.h>
#include <string.h>

// Open /dev/dsp
// set it to stereo
// read samples from stdin 
// dump them to /dev/dsp

// #define USE_UDP

#define BS 128
#define BUFFER_SIZE BS*4
unsigned char buffer[BUFFER_SIZE];

void error_message(char* name) {
    fprintf(stderr, 
      "\nUsage: %s -f <fragments> -s <log_fragment_size> -r <sample_rate> -h <server_name>\n\n",
            name);
    exit(1);
}

int main(int argc, char *argv[]) {
  int handle;
  int tmp;
 
  int sockfd, portno;
  struct sockaddr_in serv_addr;
  struct hostent *server;
  char *servername;
#if defined(USE_UDP)
  int n;
  char message[4];
  socklen_t sockaddr_length = sizeof(struct sockaddr_in);
  struct sockaddr from;
  struct sockaddr * __restrict__ fromp = &from;
#endif

  int sampling_rate, fragments, log_frag;

  if (argc == 1) error_message(argv[0]);

  if (strcmp(argv[1], "-f"))
      error_message(argv[0]);
  else 
    fragments = atoi(argv[2]);

  if (strcmp(argv[3], "-s"))
      error_message(argv[0]);
  else 
    log_frag = atoi(argv[4]);

  if (strcmp(argv[5], "-r"))
      error_message(argv[0]);
  else 
    sampling_rate = atoi(argv[6]);

  if (strcmp(argv[7], "-h"))
      error_message(argv[0]);
  else 
    servername = argv[8];

  /* Open the device for writing */
  handle=open("/dev/dsp",O_WRONLY);
  if (handle == -1) {
    fprintf(stderr, "Error when writing to /dev/dsp");
    return EXIT_FAILURE;
  }

  tmp = (fragments << 16) | log_frag;
  ioctl(handle,SNDCTL_DSP_SETFRAGMENT,&tmp);

  /* Set output format. Some usual values are:
     AFMT_S16_LE - 16-bit signed, little endian
     AFMT_U8     - 8-bit unsigned */
  tmp=AFMT_S16_LE;
  ioctl(handle,SNDCTL_DSP_SETFMT,&tmp);

  /* Set number of channels. 0=mono, 1=stereo */
  tmp=1;
  ioctl(handle,SNDCTL_DSP_STEREO,&tmp);

  tmp = sampling_rate;
  ioctl(handle, SNDCTL_DSP_SPEED, &tmp);

  // Set up network connection

  portno = 3003; // !!!!!!!!!!!!!!!!1

#if defined(USE_UDP)
  //  fprintf(stderr, "About to create socket\n");
  sockfd = socket(AF_INET, SOCK_DGRAM, 0);
  //  fprintf(stderr, "Socket created\n");
#else
  sockfd = socket(AF_INET, SOCK_STREAM, 0);
  {
    int sock, ret, sockopt;
    struct linger fix_ling;

    sock = sockfd;
    if (sock < 0) exit(-1);

    /* don't leave the socket in a TIME_WAIT state if we close the
       connection */
    fix_ling.l_onoff = 1;
    fix_ling.l_linger = 0;
    ret = setsockopt(sock, SOL_SOCKET, SO_LINGER, &fix_ling, sizeof(fix_ling));
    if (ret < 0) {
      close(sock);                /* anyway the socket was created */
      exit(-2);
    }

    /* fix the socket options */
    sockopt = 1;
    ret = 
       setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, &sockopt, sizeof(sockopt));
    if (ret < 0) {
      close(sock);                /* anyway the socket was created */
      exit(-2);
    }
  }
#endif

  server = gethostbyname(servername); 
  bzero((char *) &serv_addr, sizeof(serv_addr));
  serv_addr.sin_family = AF_INET;
  bcopy((char *)server->h_addr, 
        (char *)&serv_addr.sin_addr.s_addr,
        server->h_length);
  serv_addr.sin_port = htons(portno);

#if defined(USE_UDP)
  // Initial message.  Server should be up?
  //  fprintf(stderr, "Repeating sendto()\n");
#else
// loop until we connect (active wait on purpose to avoid centrino
// processors slow down the clock)
  while (connect(sockfd, 
                 (const struct sockaddr*)&serv_addr, 
                 sizeof(serv_addr)) != 0) ;
#endif

  while (1) {

#if defined(USE_UDP)
    while(sendto(sockfd, message, 4, 0, 
        (const struct sockaddr *)&serv_addr, sockaddr_length) < 0) {
    perror("sendto():");
  }
  //  fprintf(stderr, "sendto() done\n");
    n = recvfrom(sockfd, buffer, BUFFER_SIZE, 0, fromp, &sockaddr_length);
    if (n == 0) break;
    if (n < 0) perror("recvfrom():");
#else
    if (read(sockfd, buffer, BUFFER_SIZE) == 0) break;
#endif

#if defined(BENCHMARK)
    // do not emit any sound, run as fast as possible
#else
    write(handle, buffer, BUFFER_SIZE);
#endif

  }
    
  close(sockfd);
  close(handle);
}
