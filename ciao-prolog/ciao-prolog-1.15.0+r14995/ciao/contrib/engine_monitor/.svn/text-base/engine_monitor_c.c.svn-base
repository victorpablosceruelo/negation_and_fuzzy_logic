/* C side of the engine monitor 

   Author: Jose F. Morales
*/

#include <ciao_prolog.h>
#include <task_areas.h>
#include <registers.h>
#include <alloc_defs.h>

#include <string.h>

#include <stdarg.h>

#if defined(DARWIN)
#include <OpenGL/gl.h>
#include <OpenGL/glu.h>
#include <GLUT/glut.h>
#else
#include <GL/gl.h>
#include <GL/glut.h>
#endif

#include <stdio.h>

/***************************************************************************/
/* Output window */

int window_width = 512;
int window_height = 640;

/***************************************************************************/
/* Text output routines */

//#define TEXT_FONT GLUT_BITMAP_HELVETICA_10
#define TEXT_FONT GLUT_BITMAP_9_BY_15
#define TEXT_HEIGHT 16

int curr_x = 0;
int curr_y = 0;

void set_curr(int x, int y) {
  curr_x = x;
  curr_y = y;
}

void topleft_curr() {
  set_curr(16, window_height-16);
}

void newline() {
  curr_y -= TEXT_HEIGHT;
}

void output(const char *string) {
  int len, i;

  glRasterPos2f(curr_x, curr_y);
  len = (int) strlen(string);
  for (i = 0; i < len; i++) {
    if (string[i] == '\n') {
      newline();
      glRasterPos2f(curr_x, curr_y);
    } else {
      glutBitmapCharacter(TEXT_FONT, string[i]);
    }
  }
}

/* Like printf, but using output() to write to the OpenGL window */
#define BUF_SIZE 1024
void gl_printf(const char* format, ...) {
  char out[BUF_SIZE];
  va_list argptr;
  va_start(argptr, format);
  vsnprintf(out, BUF_SIZE, format, argptr);
  va_end(argptr);
  output(out);
}

/***************************************************************************/
/* Custom drawing operations */

int stack_width = 64;

typedef struct _stack_t stack_t;
struct _stack_t {
  /* missing: growing direction, start address */
  int top;
  int size;

  /* color to draw draw the stack */
  float color_r, color_g, color_b;

  /* name of the stack */
  char *name;
};

/* We do not show the contents of the heap yet */
void draw_stack(int x, int y, stack_t *s) {
  if (s->top > s->size) {
    /* s->top can be miscalculated due to NULL local_top */
    return;
  }

  float factor;
  if (s->size == 0) {
    factor = 1.0;
  } else {
    factor = (float)(window_height/2) / s->size;
  }

  set_curr(x, y + window_height/2 + 16);
  glColor3f(s->color_r, s->color_g, s->color_b);
  gl_printf("%s", s->name);

  int height = factor * s->size;
  int height0 = factor * s->top;
  if (height0 < height) {
    /* used space */
    glColor3f(s->color_r/2.0, s->color_g/2.0, s->color_b/2.0);
    glRecti(x+0, y+height0, x+stack_width, y+height);
    /* used space */
    glColor3f(s->color_r, s->color_g, s->color_b);
    glRecti(x+0, y+0, x+stack_width, y+height0);
  } else {
    /* used space */
    glColor3f(s->color_r, s->color_g, s->color_b);
    glRecti(x+0, y+0, x+stack_width, y+height);
  }

    //GLubyte *ptr = &ptr;
  // ptr = (GLubyte *)heap_start;
  //  glPixelZoom(1.0, (float)(window_height - 100) / height);
  // glDrawPixels(width, height, GL_LUMINANCE, GL_UNSIGNED_BYTE, ptr);

  //    glRasterPos2f ( 0.8f, 0 );
  //    glBitmap ( 32, 32, 0.0, 0.0, 0, 0, fire );
}

/***************************************************************************/

/* The goal descriptor containing the worker that we want to monitor */
goal_descriptor_t *monitored_gd;

/* Draw the heaps */
void render_scene(void)
{
  glClear(GL_COLOR_BUFFER_BIT);
  glColor3f(1.0f, 1.0f, 1.0f);

  /* Set coordinate unit equal to one screen pixel */
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluOrtho2D(0, window_width, 0, window_height);

  glRasterPos2i(0,0);

  /* Assume that we monitor the next worker */
  worker_t *w = monitored_gd->worker_registers;

  stack_t heap;
  heap.name = "Heap";
  heap.top = (char *)w->global_top - (char *)w->heap_start;
  heap.size = (char *)w->heap_end - (char *)w->heap_start;
  heap.color_r = 1.0;
  heap.color_g = 0.0;
  heap.color_b = 0.0;

  stack_t stack;
  stack.name = "Stack";
  stack.top = (char *)w->local_top - (char *)w->stack_start;
  stack.size = (char *)w->stack_end - (char *)w->stack_start;
  stack.color_r = 1.0;
  stack.color_g = 1.0;
  stack.color_b = 0.0;

  /* choice grows backwards */
  stack_t choice;
  choice.name = "Choice";
  choice.top = (char *)w->choice_start - (char *)w->node;
  choice.size = (char *)w->choice_start - (char *)w->choice_end;
  choice.color_r = 0.0;
  choice.color_g = 0.5;
  choice.color_b = 1.0;

  stack_t trail;
  trail.name = "Trail";
  trail.top = (char *)w->trail_top - (char *)w->trail_start;
  trail.size = (char *)w->trail_end - (char *)w->trail_start;
  trail.color_r = 0.0;
  trail.color_g = 1.0;
  trail.color_b = 0.0;

  int x = 0;
  draw_stack(x, 0, &heap); x += stack_width+16;
  draw_stack(x, 0, &stack); x += stack_width+16;
  draw_stack(x, 0, &choice); x += stack_width+16;
  draw_stack(x, 0, &trail); x += stack_width+16;

  {
    glColor3f(0.8f, 0.5f, 1.0f);

    topleft_curr();
    gl_printf("ellapsed time: %d ms\n", glutGet(GLUT_ELAPSED_TIME));
    gl_printf("worker: %p\n", w);
    // gl_printf("heap_start: %p\n", heap_start);
    gl_printf("heap.size: 0x%x\n", heap.size);
    gl_printf("heap.top: 0x%x\n", heap.top);
    gl_printf("stack.size: 0x%x\n", stack.size);
    gl_printf("stack.top: 0x%x\n", stack.top);
    gl_printf("choice.size: 0x%x\n", choice.size);
    gl_printf("choice.top: 0x%x\n", choice.top);
    gl_printf("trail.size: 0x%x\n", trail.size);
    gl_printf("trail.top: 0x%x\n", trail.top);
    /* TODO: IMHO, this should be per/thread and not global */
    gl_printf("GC count: %d\n", ciao_statistics.gc_count);
    gl_printf("\n");
    gl_printf("program (+atoms): %ld bytes\n", mem_prog_count);
    gl_printf("# of atoms: %ld\n", ciao_atoms->count);
    gl_printf("# of preds: %ld\n", num_of_predicates);
  }

  glutSwapBuffers();
}

/* callback for glutDisplayFunc */
void display() {
  glClearColor(0.0, 0.0, 0.0, 1.0);
  glClear(GL_COLOR_BUFFER_BIT);
  render_scene();
  glFlush();
}

void resize_scene(int width, int height) {
  window_width = width;
  window_height = height;

  glViewport(0, 0, window_width, window_height);

  /* Set coordinate unit equal to one screen pixel */
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluOrtho2D(0, window_width, 0, window_height);

  display();
}

void setup_timer();

void timer_callback() {
  display();
  setup_timer();
}

#define TIMERMSECS 10

void setup_timer() {
  glutTimerFunc(TIMERMSECS, timer_callback, 0);
}

bool_t prolog_monitor_eng(Arg)
     Argdecl;
{
  ERR__FUNCTOR("engine_monitor:monitor_eng", 1);
  goal_descriptor_t *this_goal;

  DEREF(X(0), X(0));
  if (!TagIsSmall(X(0)))
    {BUILTIN_ERROR(TYPE_ERROR(NUMBER), X(0), 1);}
  else  {
    monitored_gd = TermToGoalDesc(X(0));
  }
  printf("Monitoring worker %p\n", monitored_gd->worker_registers);
  return TRUE;
}

void begin_monitor() {
  char argv[]={"Ciao Engine Monitor"};
  int argc=1;

  glutInit(&argc, (char **)argv);
  glutInitWindowSize(window_width, window_height);
  glutInitDisplayMode(GLUT_SINGLE | GLUT_RGB);
  glutCreateWindow("Ciao Engine Monitor");
  glutDisplayFunc(display);
  glutReshapeFunc(resize_scene);
  setup_timer();
  glutMainLoop(); // Infinite event loop
}
