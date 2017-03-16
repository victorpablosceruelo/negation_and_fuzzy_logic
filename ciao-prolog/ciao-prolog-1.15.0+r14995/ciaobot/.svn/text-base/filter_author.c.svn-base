/*
** filter_author.c
** 
** Made by (Edison Mera)
** Login   <edison@clip.dia.fi.upm.es>
** 
** Started on  Thu Oct  5 15:55:09 2006 Edison Mera
** Last update Sun Oct 15 17:11:08 2006 Edison Mera
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define AUTHOR_LENGTH   256

#define MAX_NUM_AUTHORS 128

void add_author(char *author, int author_length, char **authors)
{
  int i = 0;

  while(authors[i] != NULL) {
    if(!strcmp(author, authors[i]))
      return;
    if(i >= MAX_NUM_AUTHORS - 1)
      break;
    i++;
  }
  authors[i] = (char *)malloc(author_length*sizeof(char));
  strcpy(authors[i], author);
  printf("%s\n",author);
  authors[i+1] = NULL;
}

void free_authors(char **authors)
{
  int i = 0;
  for(i = 0; authors[i] != NULL; i++) 
    free(authors[i]);
}

int main(void)
{
  char buffer[AUTHOR_LENGTH + 1];
  char *ini_author="<author>";
  char *end_author="</author>\n";
  char *author;
  char *authors[MAX_NUM_AUTHORS + 1];
  int top_name = 0;
  int is_first_author = 1;
  size_t ini_author_length;
  size_t end_author_length;
  size_t author_length;
  size_t buffer_length;

  authors[0] = NULL;

  while(fgets(buffer, AUTHOR_LENGTH, stdin) != NULL) {
    ini_author_length = strlen(ini_author);
    end_author_length = strlen(end_author);
    buffer_length = strlen(buffer);

    if(!strncmp(buffer, ini_author, ini_author_length) &&
       !strncmp(buffer + buffer_length - end_author_length, end_author,
		end_author_length)) {
      author_length = buffer_length - ini_author_length - end_author_length;

      if(author_length > 0) {
	author = buffer + ini_author_length;
	author[author_length] = '\0';

      /* Ignore first line (is not member of the current author list) */
	if(is_first_author)
	  is_first_author = 0;
	else {
	  add_author(author, author_length, authors);
	}
      }
    }
  }
  free_authors(authors);
  return 0;
}
