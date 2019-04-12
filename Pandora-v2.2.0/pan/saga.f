      subroutine SAGA
     $(SYM,CALLER,STOP)
C
C     Rudolf Loeser, 1980 Feb 13
C---- Provides an error printout involving the Element Tables.
C     (This is version 2 of SAGA.)
C     !DASH
      save
C     !DASH
      integer J, LENELE, LENSYM, LUEO
      logical STOP
      character CALLER*(*), SYM*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
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
      external  MESHED, ABORT, LINER, HI, BYE
      intrinsic len
C     !EJECT
C
      call HI ('SAGA')
C     !BEG
      if(STOP) then
        call MESHED (CALLER//'/SAGA', 1)
      else
        call MESHED (CALLER//'/SAGA', 3)
      end if
C
      LENSYM = len(SYM)
      write (LUEO,100) CALLER,SYM,LENSYM
  100 format(' ','Error in ',A10,5X,
     $           'Entry expected in Element Tables but not found: ',
     $           '[',A8,']',I10)
      call LINER    (2, LUEO)
C
      do 102 J = 1,NMT
        LENELE = len(ELSYM(J))
        write (LUEO,101) ELSYM(J),LENELE,LATNO(J),ELABD(J),ELCHI(J),
     $                   ELLU1(J),ELLU2(J),ELABL(J),ELDEF(J),LDEFR(J)
  101   format(' ',9X,'[',A8,']',I2,I4,1P6E15.4,I4)
  102 continue
C
      if(STOP) then
        call ABORT
      end if
C     !END
      call BYE ('SAGA')
C
      return
      end
