      subroutine TALEFEN
     $(NO)
C     Rudolf Loeser, 2001 Jun 25
C---- Prints values of constant partition functions.
C     !DASH
      save
C     !DASH
      integer I, IN, J, LUM, MUM, NO, NUM
      character BLANK*1, LINE*120
C     !COM
C---- ELEMENT     as of 1998 Aug 17
      integer     NELX
      parameter   (NELX=50)
C     (Remember to recompile all users when changing NELX)
      real*8      ELABD,ELCHI,ELLU1,ELLU2,ELABL,ELDEF
      integer     LATNO,LDEFR,NMT,NMTMAX
      logical     LATEM
      character   ELSYM*3, ELSUB*3
      dimension   ELSYM(NELX),ELSUB(NELX),ELABD(NELX),ELCHI(NELX),
     $            ELLU1(NELX),ELLU2(NELX),ELABL(NELX),ELDEF(NELX),
     $            LATNO(NELX),LDEFR(NELX),LATEM(NELX)
C
      common      /ELEMNT0/ NMT,NMTMAX
      common      /ELEMNT1/ ELSYM,ELSUB
      common      /ELEMNT2/ ELABD,ELCHI,ELLU1,ELLU2,ELABL,ELDEF
      common      /ELEMNT3/ LATNO,LDEFR
      common      /ELEMNT4/ LATEM
C
C     Element data tables:
C             ELSYM - element symbol;
C             ELSUB - (Scratch storage for I.D. symbols);
C             ELABD - abundance (w.r.t. Hydrogen);
C             ELCHI - Chi, i.e. ionization potential;
C             ELLU1 - U-I partition function;
C             ELLU2 - U-II partition function;
C             ELABL - logarithmic abundance;
C             ELDEF - defaults values of logarithmic abundance;
C             LATNO - atomic number; and
C             LDEFR - default values sources codes.
C             LATEM - "metal" designator
C     .
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external  LINER, JOUST, HI, BYE
      intrinsic mod
C
      dimension LUM(3)
C     !EJECT
C
      call HI ('TALEFEN')
C     !BEG
      if(NO.gt.0) then
        write (NO,100)
  100   format(' ',3(12X,'U-I',6X,'U-II',15X))
        call LINER (1,NO)
C
        call JOUST (NMT,LUM)
        J    = -40
        IN   = 1
        NUM  = 1
        MUM  = NUM
        LINE = BLANK
C
        do 103 I = 1,NMT
          if(MUM.le.NMT) then
            J = J+40
            write (LINE(J+1:J+25),101) ELSYM(MUM),ELLU1(MUM),ELLU2(MUM)
  101       format(A3,2X,2F10.3)
            MUM = MUM+LUM(IN)
            IN  = IN+1
            if(mod(I,3).eq.0) then
              write (NO,102) LINE
  102         format(' ',A120)
              J    = -40
              IN   = 1
              NUM  = NUM+1
              MUM  = NUM
              LINE = BLANK
            end if
          end if
  103   continue
C
        if(J.ge.0) then
          write (NO,102) LINE
        end if
C
      end if
C     !END
      call BYE ('TALEFEN')
C
      return
      end
