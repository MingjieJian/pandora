      subroutine ICEMAN
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1980 Jul 22
C     Revised RL/SGK Apr  9 2014 
C---- Allocates scratch storage for BULLET.
C     (This is version 3 of ICEMAN.)
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, N, NNMT
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
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
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C     !EJECT
C
      call HI ('ICEMAN')
C     !BEG
      call WGET (IS ,CALLER)
C
      NNMT = N*NMT
C
      IN( 1) = IS
      IN( 2) = IN( 1)+NNMT
      IN( 3) = IN( 2)+N
      IN( 4) = IN( 3)+N
      IN( 5) = IN( 4)+NNMT
      IN( 6) = IN( 5)+NNMT
      IN( 7) = IN( 6)+NMT
      IN( 8) = IN( 7)+NNMT
      IN( 9) = IN( 8)+NNMT
      IN(10) = IN( 9)+NNMT
      IN(11) = IN(10)+N
C
      IN(12) = IN(11)+N
      IN(13) = IN(12)+N
      MUX    = IN(13)+N
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('ICEMAN')
C
      return
      end
