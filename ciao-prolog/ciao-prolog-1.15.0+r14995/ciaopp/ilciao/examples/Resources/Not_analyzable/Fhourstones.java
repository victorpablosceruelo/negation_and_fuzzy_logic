// This software is copyright (c) 1996-2005 by
//      John Tromp
//      Insulindeweg 908
//      1095 DX Amsterdam
//      Netherlands
// E-mail: tromp@cwi.nl
//
// This notice must not be removed.
// This software must not be sold for profit.
// You may redistribute if your distributees have the
// same rights and restrictions.

package examples;
import java.io.*;
import java.text.DecimalFormat;

public class Fhourstones extends TransGame {
  static final int BOOKPLY = 0;  // full-width search up to this depth
  static final int REPORTPLY = -1;
  int history[][] = new int[2][SIZE1];
  long nodes, msecs;
  
  void inithistory()
  {
    for (int side=0; side<2; side++)
      for (int i=0; i<(WIDTH+1)/2; i++)
        for (int h=0; h<H1/2; h++)
          history[side][H1*i+h] = history[side][H1*(WIDTH-1-i)+HEIGHT-1-h] =
          history[side][H1*i+HEIGHT-1-h] = history[side][H1*(WIDTH-1-i)+h] =
            4+Math.min(3,i) + Math.max(-1,Math.min(3,h)-Math.max(3-i,0))
            + Math.min(3,Math.min(i,h)) + Math.min(3,h);
  }

  int ab(int alpha, int beta)
  {
    nodes++;
    if (nplies == SIZE-1) // one move left
      return DRAW; // by assumption, player to move can't win
    int side, otherside;
    otherside = (side = nplies & 1) ^ 1;
    long other = color[otherside];
    int i,nav,av[] = new int[WIDTH];
    long newbrd;
    boolean winontop;
    for (i = nav = 0; i < WIDTH; i++) {
      newbrd = other | (1L << height[i]); // check opponent move
      if (!islegal(newbrd)) 
        continue;
      winontop = islegalhaswon(other | (2L << height[i]));
      if (haswon(newbrd)) { // immediate threat
        if (winontop) // can't stop double threat
          return LOSS;
        nav = 0; // forced move
        av[nav++] = i;
        while (++i < WIDTH)
          if (islegalhaswon(other | (1L << height[i])))
            return LOSS;
        break;
      }
      if (!winontop)
        av[nav++] = i;
    }
    if (nav == 0)
      return LOSS;
    if (nplies == SIZE-2) // two moves left
      return DRAW; // opponent has no win either
    int score;
    if (nav == 1) {
      makemove(av[0]);
      score = LOSSWIN-ab(LOSSWIN-beta,LOSSWIN-alpha);
      backmove();
      return score;
    }
    int ttscore = transpose();
    if (ttscore != UNKNOWN) {
      if (ttscore == DRAWLOSS) {
        if ((beta = DRAW) <= alpha)
          return ttscore;
      } else if (ttscore == DRAWWIN) {
        if ((alpha = DRAW) >= beta)
          return ttscore;
      } else return ttscore; // exact score
    }
    int hashindx = htindex;
    int hashlock = lock;
    long poscnt = posed;
    int besti=0,j,l,sc;
    int v,val;
    score = LOSS;
    for (i = 0; i < nav; i++) {
      val = history[side][height[av[l = i]]];
      for (j = i+1; j < nav; j++) {
        v = history[side][height[av[j]]];
        if (v > val) {
          val = v; l = j;
        }
      }
      for (j = av[l]; l>i; l--)
        av[l] = av[l-1];
      makemove(av[i] = j);
      sc = LOSSWIN-ab(LOSSWIN-beta,LOSSWIN-alpha);
      backmove();
      if (sc > score) {
        besti = i;
        if ((score=sc) > alpha && nplies >= BOOKPLY && (alpha=sc) >= beta) {
          if (score == DRAW && i < nav-1)
            score = DRAWWIN;
          if (besti > 0) {
            for (i = 0; i < besti; i++)
              history[side][height[av[i]]]--; // punish worse
            history[side][height[av[besti]]] += besti;
          }
          break;
        }
      }
    }
    if (score == LOSSWIN-ttscore) // combine < and >
      score = DRAW;
    poscnt = posed - poscnt;
    int work;
    for (work=0; (poscnt>>=1) != 0; work++) ; // work=log #positions stored
    transtore(hashindx, hashlock, score, work);
    if (nplies <= REPORTPLY) {
      System.out.println(toString() + "#-<=>+".charAt(score) + work);
    }
    return score;
  }

  int solve()
  {
    int i, side = nplies & 1, otherside = side ^ 1;
    nodes = 0L;
    msecs = 1L;
    if (haswon(color[otherside]))
      return LOSS;
    for (i = 0; i < WIDTH; i++)
      if (islegalhaswon(color[side] | (1L << height[i])))
        return WIN;
    inithistory();
    msecs = System.currentTimeMillis();
    int score = ab(LOSS, WIN);
    msecs = System.currentTimeMillis() + 1 - msecs; // prevent division by 0
    return score;
  }

  public static void main(String argv[])
  {
    System.out.println("Fhourstones 3.1 (Java)");
    System.out.println("Boardsize = " + WIDTH + "x" + HEIGHT);
    System.out.println("Using " + TRANSIZE + " transposition table entries.");
    Fhourstones c4 = new Fhourstones();
    BufferedReader dis = new BufferedReader(new InputStreamReader(System.in));
    DecimalFormat df = new DecimalFormat("######.###");
 
    for (;;) {
      String line=null;
      try {
        line = dis.readLine();
      } catch (IOException e) {
        System.out.println(e);
        System.exit(0);
      }
      if (line == null)
        break;
      c4.reset();
      for (int i=0; i < line.length(); i++)
        c4.makemove(line.charAt(i) - '1');
      System.out.println("\nSolving " + c4.nplies + "-ply position after "
        + c4.toString() + " . . .");

      c4.emptyTT();
      int result = c4.solve();
      long poscnt = c4.posed;
      int work;
      for (work=0; (poscnt>>=1) != 0; work++) ; //work = log #transpositions
      System.out.println("score = " + result + " (" +
         "#-<=>+".charAt(result) + ")  work = " + work);
      System.out.println("" + c4.nodes + " pos / " + c4.msecs +
        " msec = " + df.format((double)c4.nodes/c4.msecs) + " Kpos/sec");
      System.out.println(c4.htstat());
    }
  }
}

/// TransGame.java

class TransGame extends Game {
  static final int LOCKSIZE = 26;
  static final int SCORELOCKSIZE = 3+LOCKSIZE;
    static final int TRANSIZE = 60;  // 8306069; // too big !!!
  // should be a prime no less than about 2^{SIZE1-LOCKSIZE}
  static final int SYMMREC = 10; // symmetry normalize first SYMMREC moves
  static final int UNKNOWN = 0;
  static final int LOSS = 1;
  static final int DRAWLOSS = 2;
  static final int DRAW = 3;
  static final int DRAWWIN = 4;
  static final int WIN = 5;
  static final int LOSSWIN = LOSS+WIN;

  static final int SCORELOCKMASK = (1<<SCORELOCKSIZE)-1;
  static final int LOCKMASK = (1<<LOCKSIZE)-1;
  protected int ht[]; // hash entries
  protected int htindex, lock;
  
  protected long posed; // counts transtore calls
  
  public TransGame()
  {
    super();
    ht = new int[2*TRANSIZE];
  }
  
  void emptyTT()
  {
    int i, h, work;

    for (i=0; i<2*TRANSIZE; i++)
      ht[i] = 0;
    posed = 0L;
  }
  
  void hash()
  {
    long htemp = positioncode();
    if (nplies < SYMMREC) { // try symmetry recognition by reversing columns
      long htemp2 = 0L;
      for (long htmp=htemp; htmp!=0; htmp>>=H1)
        htemp2 = htemp2<<H1 | (htmp & COL1);
      if (htemp2 < htemp)
        htemp = htemp2;
    }
    lock = (int)(htemp >> (SIZE1-26));
    htindex = 2*(int)(htemp % TRANSIZE);
  }
  
  int transpose()
  {
    hash();
    int he0 = ht[htindex];
    int he1 = ht[htindex+1];
    if ((he0 & LOCKMASK) == lock) // biglock 
      return he1 >>> SCORELOCKSIZE; // bigscore
    if ((he1 & LOCKMASK) == lock) // newlock
      return (he1 >> LOCKSIZE) & 7; // newscore
    return UNKNOWN;
  }

  void transtore(int x, int lock, int score, int work)
  {
    posed++;
    int he0 = ht[x];
    int he1 = ht[x+1];
    int biglock = he0 & LOCKMASK;
    if (biglock == lock || work >= (he0 >>> LOCKSIZE)) {
      ht[x] = (work << LOCKSIZE) | lock;
      ht[x+1] = (score << SCORELOCKSIZE) | (he1 & SCORELOCKMASK);
    } else {
      ht[x+1] = ((he1 >>> SCORELOCKSIZE) << 3 | score) << LOCKSIZE | lock;
    }
  }
  
  String htstat()      /* some statistics on hash table performance */
  {
    int total, i;
    StringBuffer buf = new StringBuffer();
    int typecnt[];                // bound type stats
    DecimalFormat df = new DecimalFormat("######.###");

    typecnt = new int[8];
    for (i=0; i<8; i++)
      typecnt[i] = 0;
    for (i=0; i<2*TRANSIZE; i+=2) {
      int he0 = ht[i];
      int he1 = ht[i+1];
      int biglock = he0 & LOCKMASK;
      int bigscore = he1 >>> SCORELOCKSIZE;
      int newlock = he1 & LOCKMASK;
      int newscore = (he1 >> LOCKSIZE) & 7;
      if (biglock != 0)
        typecnt[bigscore]++;
      if (newlock != 0)
        typecnt[newscore]++;
    }
    for (total=0,i=LOSS; i<=WIN; i++)
      total += typecnt[i];
    if (total > 0)
      buf.append("- "+df.format(typecnt[LOSS]/(double)total) +
       " < "+df.format(typecnt[DRAWLOSS]/(double)total) +
       " = "+df.format(typecnt[DRAW]/(double)total) +
       " > " + df.format(typecnt[DRAWWIN]/(double)total) +
       " + " + df.format(typecnt[WIN]/(double)total));
    return buf.toString();
  }
}
/// Game.java

class Game {
  static long color[];  // black and white bitboard
  static final int WIDTH = 7;
  static final int HEIGHT = 6;
// bitmask corresponds to board as follows in 7x6 case:
//  .  .  .  .  .  .  .  TOP
//  5 12 19 26 33 40 47
//  4 11 18 25 32 39 46
//  3 10 17 24 31 38 45
//  2  9 16 23 30 37 44
//  1  8 15 22 29 36 43
//  0  7 14 21 28 35 42  BOTTOM
  static final int H1 = HEIGHT+1;
  static final int H2 = HEIGHT+2;
  static final int SIZE = HEIGHT*WIDTH;
  static final int SIZE1 = H1*WIDTH;
  static final long ALL1 = (1L<<SIZE1)-1L; // assumes SIZE1 < 63
  static final int COL1 = (1<<H1)-1;
  static final long BOTTOM = ALL1 / COL1; // has bits i*H1 set
  static final long TOP = BOTTOM << HEIGHT;

  int moves[],nplies;
  byte height[]; // holds bit index of lowest free square
  
  public Game()
  {
    color = new long[2];
    height = new byte[WIDTH];
    moves = new int[SIZE];
    reset();
  }
  
  void reset()
  {
    nplies = 0;
    color[0] = color[1] = 0L;
    for (int i=0; i<WIDTH; i++)
      height[i] = (byte)(H1*i);
  }

  public long positioncode()
  {
    return color[nplies&1] + color[0] + color[1] + BOTTOM;
// color[0] + color[1] + BOTTOM forms bitmap of heights
// so that positioncode() is a complete board encoding
  }

  public String toString()
  {
    StringBuffer buf = new StringBuffer();

    for (int i=0; i<nplies; i++)
      buf.append(1+moves[i]);
    if (true) return buf.toString(); // remove to get board + info printed
    buf.append("\n");
    for (int w=0; w<WIDTH; w++)
      buf.append(" "+(w+1));
    buf.append("\n");
    for (int h=HEIGHT-1; h>=0; h--) {
      for (int w=h; w<SIZE1; w+=H1) {
        long mask = 1L<<w;
        buf.append((color[0]&mask)!= 0 ? " @" :
                   (color[1]&mask)!= 0 ? " 0" : " .");
      }
      buf.append("\n");
    }
    if (haswon(color[0]))
      buf.append("@ won\n");
    if (haswon(color[1]))
      buf.append("O won\n");
    return buf.toString();
  }

  // return whether columns col has room
  final boolean isplayable(int col)
  {
    return islegal(color[nplies&1] | (1L << height[col]));
  }

  // return whether newboard lacks overflowing column
  final boolean islegal(long newboard)
  {
    return (newboard & TOP) == 0;
  }

  // return whether newboard is legal and includes a win
  final boolean islegalhaswon(long newboard)
  {
    return islegal(newboard) && haswon(newboard);
  }

  // return whether newboard includes a win
  final boolean haswon(long newboard)
  {
    long y = newboard & (newboard>>HEIGHT);
    if ((y & (y >> 2*HEIGHT)) != 0) // check diagonal \
      return true;
    y = newboard & (newboard>>H1);
    if ((y & (y >> 2*H1)) != 0) // check horizontal -
      return true;
    y = newboard & (newboard>>H2); // check diagonal /
    if ((y & (y >> 2*H2)) != 0)
      return true;
    y = newboard & (newboard>>1); // check vertical |
    return (y & (y >> 2)) != 0;
  }

  void backmove()
  {
    int n;

    n = moves[--nplies];
    color[nplies&1] ^= 1L<<--height[n];
  }

  void makemove(int n) 
  {
    color[nplies&1] ^= 1L<<height[n]++;
    moves[nplies++] = n;
  }
}

/////////////////////////////////////////////////////////////////////////////

// Fhourstones 3.0 Board Logic
// Copyright 2000-2004 John Tromp

class Connect4 {
  static long color[];  // black and white bitboard
  static final int WIDTH = 7;
  static final int HEIGHT = 6;
// bitmask corresponds to board as follows in 7x6 case:
//  .  .  .  .  .  .  .  TOP
//  5 12 19 26 33 40 47
//  4 11 18 25 32 39 46
//  3 10 17 24 31 38 45
//  2  9 16 23 30 37 44
//  1  8 15 22 29 36 43
//  0  7 14 21 28 35 42  BOTTOM
  static final int H1 = HEIGHT+1;
  static final int H2 = HEIGHT+2;
  static final int SIZE = HEIGHT*WIDTH;
  static final int SIZE1 = H1*WIDTH;
  static final long ALL1 = (1L<<SIZE1)-1L; // assumes SIZE1 < 63
  static final int COL1 = (1<<H1)-1;
  static final long BOTTOM = ALL1 / COL1; // has bits i*H1 set
  static final long TOP = BOTTOM << HEIGHT;

  int moves[],nplies;
  byte height[]; // holds bit index of lowest free square
  
  public Connect4()
  {
    color = new long[2];
    height = new byte[WIDTH];
    moves = new int[SIZE];
    reset();
  }
  
  void reset()
  {
    nplies = 0;
    color[0] = color[1] = 0L;
    for (int i=0; i<WIDTH; i++)
      height[i] = (byte)(H1*i);
  }

  public long positioncode()
  {
    return 2*color[0] + color[1] + BOTTOM;
// color[0] + color[1] + BOTTOM forms bitmap of heights
// so that positioncode() is a complete board encoding
  }

  public String toString()
  {
    StringBuffer buf = new StringBuffer();

    for (int i=0; i<nplies; i++)
      buf.append(1+moves[i]);
    buf.append("\n");
    for (int w=0; w<WIDTH; w++)
      buf.append(" "+(w+1));
    buf.append("\n");
    for (int h=HEIGHT-1; h>=0; h--) {
      for (int w=h; w<SIZE1; w+=H1) {
        long mask = 1L<<w;
        buf.append((color[0]&mask)!= 0 ? " @" :
                   (color[1]&mask)!= 0 ? " 0" : " .");
      }
      buf.append("\n");
    }
    if (haswon(color[0]))
      buf.append("@ won\n");
    if (haswon(color[1]))
      buf.append("O won\n");
    return buf.toString();
  }

  // return whether columns col has room
  final boolean isplayable(int col)
  {
    return islegal(color[nplies&1] | (1L << height[col]));
  }

  // return whether newboard lacks overflowing column
  final boolean islegal(long newboard)
  {
    return (newboard & TOP) == 0;
  }

  // return whether newboard is legal and includes a win
  final boolean islegalhaswon(long newboard)
  {
    return islegal(newboard) && haswon(newboard);
  }

  // return whether newboard includes a win
  final boolean haswon(long newboard)
  {
    long y = newboard & (newboard>>HEIGHT);
    if ((y & (y >> 2*HEIGHT)) != 0) // check diagonal \
      return true;
    y = newboard & (newboard>>H1);
    if ((y & (y >> 2*H1)) != 0) // check horizontal -
      return true;
    y = newboard & (newboard>>H2); // check diagonal /
    if ((y & (y >> 2*H2)) != 0)
      return true;
    y = newboard & (newboard>>1); // check vertical |
    return (y & (y >> 2)) != 0;
  }

  void backmove()
  {
    int n;

    n = moves[--nplies];
    color[nplies&1] ^= 1L<<--height[n];
  }

  void makemove(int n) 
  {
    color[nplies&1] ^= 1L<<height[n]++;
    moves[nplies++] = n;
  }

  public static void main(String argv[])
  {
    Connect4 c4;
    String line;
    int col=0, i, result;
    long nodes, msecs;

    c4 = new Connect4();
    c4.reset();
    BufferedReader dis = new BufferedReader(new InputStreamReader(System.in));

    for (;;) {
      System.out.println("position " + c4.positioncode() + " after moves " + c4 + "enter move(s):");
      try {
        line = dis.readLine();
      } catch (IOException e) {
        System.out.println(e);
        System.exit(0);
        return;
      }
      if (line == null)
        break;
      for (i=0; i < line.length(); i++) {
        col = line.charAt(i) - '1';
        if (col >= 0 && col < WIDTH && c4.isplayable(col))
          c4.makemove(col);
      }
    }
  }
}

/////////////////////////////////////////////////////////////////////////////////////////////
