/* for compiling in pizarro: gcc test_sockets.c -lsocket -lnsl*/
#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <unistd.h>


const int SERVER_PORT = 5010 ;


int com_pipe[ 2 ] ;


int server ( struct sockaddr_in  *my_addr , socklen_t addrlen )
{
  int server        = -1 ;
  int server_client = -1 ;

  printf( "Server: Starting...\n" ) ;
  
  if ( (server = socket( PF_INET, SOCK_STREAM, 0 )) < 0 )
    {
      perror( "Server: Cannot create server socket" ) ;
      return -1 ;
    }

  if ( bind( server, (struct sockaddr *)my_addr , addrlen ) )
    {
      perror( "bind:" ) ;
      return -1 ;
    }
  
  listen( server , 5 ) ;

  {
    int foo ;
    write( com_pipe[ 1 ] , &foo , sizeof( int ) ) ;
  }
  
  printf( "Server: Accepting 1 conexion\n" ) ;

  if ( (server_client = accept( server , (struct sockaddr *)my_addr , &addrlen )) < 0 )
    {
      perror( "Server: Accepting" ) ;
      return -1 ;
    }

  printf( "Server: Conexion accepted\n" ) ;

  {
    int  len ;
    char buf[ 16 ] ;

    printf( "Server: Getting message >>" ) ;    

    while ( (len = read( server_client , buf, sizeof(buf) - 1)) > 0 )
      {
	buf[ 15 ] = 0 ;
	printf( buf ) ;
      }
    
    printf( "<< till here (Server)\n" ) ;    
  }

  close( server ) ;


  {
    int foo ;
    write( com_pipe[ 1 ] , &foo , sizeof( int ) ) ;
  }
    
  return 0 ;
}      


int init_sockaddr ( struct sockaddr_in *name , const char *hostname , 
		     unsigned short int port )
{
  struct hostent *hostinfo;

  name -> sin_family = AF_INET ;
  name -> sin_port   = htons( port ) ;
  hostinfo           = gethostbyname( hostname ) ;
  
  if ( hostinfo == NULL )
    {
      perror( "gethostbyname: " ) ;
      return -1 ;
    }

  name -> sin_addr = *(struct in_addr *) hostinfo -> h_addr ;
  
  return 0 ;
}


int client ( struct sockaddr_in  *my_addr , socklen_t addrlen )
{
  int  client  = -1 ;
  char host[]  = "www.clip.dia.fi.upm.es\0" ; /*"www.yahoo.com" ; */

  char buf[4096] = "GET /index.html HTTP/1.0 Accept: */*\r\n\0" ;
/*
  char buf[4096] = "GET http://www.clip.dia.fi.upm.es/index.html HTTP/1.0 "
		   "User-Agent: Mozilla/4.0 (compatible; MSIE 4.01; Windows NT) "
		   "Accept-Encoding:gzip, deflate "
		   "Accept-Language:en-us,fr-be;q=0.5 "
                   "Pragma:No-Cache "
		   "Proxy-Connection:Keep-Alive "
		   "Accept: * /*\r\n\0" ;
*/
  printf( "Client: Starting...\n" ) ;

  printf( "Client: Waiting for server to be initialized\n" ) ;

  {
    int foo ;
    read( com_pipe[ 0 ] , &foo , sizeof( int ) ) ;
  }
  
  if ( (client = socket( PF_INET, SOCK_STREAM, 0 )) < 0 )
    {
      perror( "Client: Cannot create server socket" ) ;
      return -1 ;
    }

  if ( connect( client ,(struct sockaddr *) my_addr , addrlen ) )
    {
      perror( "Client: Connect" ) ;
      close( client ) ;
      return -1 ;
    }

  {
    char mini_buf[] = "Hello world! by sockets\0" ;
    printf( "Client: Sending: >>>%s<<<\n" , mini_buf ) ;

    write( client , mini_buf , sizeof( mini_buf ) ) ;
    printf( "Client: Sended\n" ) ;
  }

  close( client ) ;

  {
    int foo ;
    read( com_pipe[ 0 ] , &foo , sizeof( int ) ) ;
  }
  

  
  printf( "\n\n---- Starting gethostbyname test ----\n\n" ) ;
  

  if ( (client = socket( PF_INET, SOCK_STREAM, 0 )) < 0 )
    {
      perror( "Client: Cannot create server socket" ) ;
      return -1 ;
    }

  if ( init_sockaddr( my_addr , host , 80 ) )
    {
      printf( "Client: Error looking up for host: %s\n" , host ) ;
      return -1 ;
    }

  printf( "Client: Trying to connect to %s\n" , host ) ;

  if ( connect( client ,(struct sockaddr *) my_addr , addrlen ) )
    {
      perror( "Client: Connect" ) ;
      close( client ) ;
      return -1 ;
    }
  
  printf( "Client: Connected. Retriving the webpage\n" ) ;
  

  {
    int  len ;

    printf( "Client: Sending request:\n---\n%s\n---\n" , buf ) ;
    
    if ( write( client , buf , strlen( buf ) ) < strlen( buf ) )
      {
	perror( "Client: writting the query" ) ;
	close( client ) ;
	return -1 ;
      }

    printf( "Client: Request sended. Waiting for answer\n" ) ;
    

    while ( (len = read( client , buf, sizeof(buf) - 1)) > 0 )
      {
	printf( "message of len: %d\n" , len ) ;
	buf[ len - 1 ] = 0 ;
	printf( buf ) ;
      }
    printf( buf ) ;
    
    if ( len < 0 )
      {
	perror( "Client: While retriving the webpage" ) ;
	close( client ) ;
	return -1 ;
      }
  }

  printf( "Client: all test passed :D\n" ) ;
  close( client ) ;
  return 0 ;
}



int main ( )
{
  socklen_t        addrlen ;
  struct sockaddr_in my_addr;
  pid_t            son_pid ;

  if ( pipe( com_pipe ) )
    {
      perror( "Could not start: Pipe" ) ;
      return -1 ;
    }
  
  addrlen = sizeof( my_addr ) ;
  

  my_addr.sin_family      = AF_INET ;
  my_addr.sin_addr.s_addr = htonl( INADDR_ANY  ) ;
  my_addr.sin_port        = htons( SERVER_PORT ) ;
 
  switch ( (son_pid = fork( )) )
    {
    case -1:
      perror( "Could not fork" ) ;
      return -1 ;
      
    case 0: /* son */
      exit( client( &my_addr , addrlen ) ) ;
      break ;

    default: /*father*/
      server( &my_addr , addrlen ) ;
      {
	int status ;
	printf( "FATHER waiting for son (%d): %d\n" , 
		son_pid,
		wait( &status ) ) ;
	/*		waitpid( son_pid , &status ) ) ;*/
      }
      break ;
    }

  
  return 0 ;
}

