      subroutine HUDRI
     $(CHI,IQUVP)
C
C     Rudolf Loeser, 1982 Jun 29
C---- Sets up a table of ionization potentials.
C     (This is version 3 of HUDRI.)
C     !DASH
      save
C     !DASH
      real*8 CHI, ONE, dummy
      integer I, ION, IQUVP, jummy
      logical VARY
      character ELE*3, qummy*3
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
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external ESSEX, HAMBURG, FRANK, HI, BYE
C
C               CHI(NMT)
      dimension CHI(*)
C     !EJECT
C
      call HI ('HUDRI')
C     !BEG
      VARY = IQUVP.gt.0
      do 100 I = 1,NMT
        if(VARY) then
          call ESSEX   (ELSYM(I),ELE,ION)
          call HAMBURG (ELE,ION,ONE,ONE,dummy,CHI(I),1)
        else
          call FRANK   (qummy,I,dummy,dummy,dummy,CHI(I),jummy)
        end if
  100 continue
C     !END
      call BYE ('HUDRI')
C
      return
      end
