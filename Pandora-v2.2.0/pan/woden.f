      subroutine WODEN
     $(INDEX,LIM)
C
C     Rudolf Loeser, 1983 Mar 22
C---- Sets up a table of entries for printing of element abundances.
C     !DASH
      save
C     !DASH
      integer I, I2, I3, INDEX, J0, J9, LIM
      logical N2, N3
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
C     !DASH
C     !EJECT
      external  HI, BYE
      intrinsic ichar
C
C               INDEX(NMT)
      dimension INDEX(*)
C
      call HI ('WODEN')
C     !BEG
      J0 = ichar(NUMBS( 1))
      J9 = ichar(NUMBS(10))
C
      LIM = 0
      do 100 I = 1,NMT
        I2 = ichar(ELSYM(I)(2:2))
        N2 = ((I2.lt.J0).or.(I2.gt.J9))
        I3 = ichar(ELSYM(I)(3:3))
        N3 = ((I3.lt.J0).or.(I3.gt.J9))
        if(N2.and.N3) then
          LIM = LIM+1
          INDEX(LIM) = I
        end if
  100 continue
C     !END
      call BYE ('WODEN')
C
      return
      end
