      subroutine GUZMAN
     $(CPR,N,NO,IS,IE)
C
C     Rudolf Loeser, 1982 Jul 01
C---- Prints, for MALAGA.
C     !DASH
      save
C     !DASH
      real*8 CPR
      integer I, IE, ION, IOP, IS, J, N, NO
      character ELE*2
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
C     !DASH
      external ESSEX, SHIM, HI, BYE
C
C               CPR(N,NMT)
      dimension CPR(N,*)
C     !EJECT
C
      call HI ('GUZMAN')
C     !BEG
      do 101 J = 1,NMT
        call ESSEX (ELSYM(J),ELE,ION)
        IOP = ION+1
        write (NO,100) ELE,IOP,ION,(CPR(I,J),I=IS,IE)
  100   format(' ',A2,1X,I2,'/',I2,3X,10F11.4)
        call SHIM  (J,5,NO)
  101 continue
C     !END
      call BYE ('GUZMAN')
C
      return
      end
