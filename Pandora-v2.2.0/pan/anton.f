      subroutine ANTON
C
C     Rudolf Loeser, 1992 Jul 30
C---- Initializes ionization potentials in the "Element" table
C     with the values from the HAMBURG routines.
C     !DASH
      save
C     !DASH
      real*8 CHI, ONE, THSND, ZERO, dummy
      integer I, ION
      character ELE*3
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
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external ESSEX, HAMBURG, HI, BYE
C
      data THSND /1.D3/
C     !EJECT
C
      call HI ('ANTON')
C     !BEG
      do 100 I = 1,NMT
        call ESSEX   (ELSYM(I),ELE,ION)
        call HAMBURG (ELE,ION,ONE,ONE,dummy,CHI,1)
        if((CHI.gt.ZERO).and.(CHI.lt.THSND)) then
          ELCHI(I) = CHI
        end if
  100 continue
C     !END
      call BYE ('ANTON')
C
      return
      end
