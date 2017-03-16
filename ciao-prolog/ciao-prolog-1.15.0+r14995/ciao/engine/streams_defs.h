/* Streams pointing to "user" -- should be shared */

extern stream_node_t *stream_user_input;                   /* Shared */
extern stream_node_t *stream_user_output;                  /* Shared */
extern stream_node_t *stream_user_error;                   /* Shared */


/* root of the stream pointers -- shared */

extern stream_node_t *root_stream_ptr;            /* Shared & locked */

/* initialization */

void init_streams();

/* operations on streams */

stream_node_t *insert_new_stream(stream_node_t *new_stream);
void update_stream(register stream_node_t *s, FILE *file);

tagged_t ptr_to_stream_noalias(Argdecl, register stream_node_t *n);
tagged_t ptr_to_stream(Argdecl, register stream_node_t *n);

/* stream predicates */

bool_t prolog_bootversion(register worker_t *w);
bool_t prolog_sourcepath(register worker_t *w);
bool_t prolog_open(register worker_t *w);
bool_t prolog_close(register worker_t *w);
bool_t prolog_unix_popen(register worker_t *w);
void ENG_perror(char *s);
bool_t prolog_current_input(register worker_t *w);
bool_t prolog_set_input(register worker_t *w);
bool_t prolog_current_output(register worker_t *w);
bool_t prolog_set_output(register worker_t *w);
bool_t prolog_get_stream(register worker_t *w);
bool_t prolog_replace_stream(register worker_t *w);
bool_t prolog_stream_code(register worker_t *w);
bool_t character_count(register worker_t *w);
bool_t line_position(register worker_t *w);
bool_t line_count(register worker_t *w);

bool_t current_stream(register worker_t *w);
bool_t nd_current_stream(register worker_t *w);
