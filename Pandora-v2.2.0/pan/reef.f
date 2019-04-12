      subroutine REEF
     $(N,PFT,NO)
C
C     Rudolf Loeser, 1982 Jun 21
C---- Sets up table of ratios of constant partition functions.
C     (This is version 3 of REEF.)
C     !DASH
      save
C     !DASH
      real*8 CPR, PFT, RU, TWO, U1, U2, dummy
      integer J, N, NO, jummy
      character qummy*3
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 3),TWO   )
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
      external DIVIDE, FRANK, SET1, MALAGA, LINER, TALEFEN, HI, BYE
C
C               PFT(N,NMT)
      dimension PFT(N,*)
C     !EJECT
C
      call HI ('REEF')
C     !BEG
      do 100 J = 1,NMT
        call FRANK  (qummy,J,dummy,U1,U2,dummy,jummy)
        call DIVIDE (U2,U1,RU)
        CPR = TWO*RU
        call SET1   (PFT(1,J),N,CPR)
  100 continue
C
      call LINER    (3,NO)
      call TALEFEN  (NO)
      call MALAGA   (NO,N,PFT)
C     !END
      call BYE ('REEF')
C
      return
      end
