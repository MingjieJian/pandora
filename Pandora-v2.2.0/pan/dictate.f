      subroutine DICTATE
     $(INDEX,NT,IPNT)
C
C     Rudolf Loeser, 1982 Jun 29
C---- Sets up an index array, for CORAL.
C     (This is version 2 of DICTATE.)
C     !DASH
      save
C     !DASH
      integer I, INDEX, ION, IOO, IPNT, NT
      character ELE*2, OLE*2
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
      external MOVEC, SORTC, HALT, ESSEX, HI, BYE
C
C               INDEX(NMT,2), IPNT(NMT)
      dimension INDEX(NMT,*), IPNT(*)
C     !EJECY
C
      call HI ('DICTATE')
C     !BEG
C---- Set up sorted table of element names
      call MOVEC   (ELSYM, 1, NMT, ELSUB, 1, NMT)
      call SORTC   (ELSUB, NMT, IPNT, 'Element symbols')
C
C---- Set up index array
      ELE = 'QQ'
      ION = 0
      NT  = 0
C
      do 100 I = 1,NMT
        OLE = ELE
        IOO = ION+1
        call ESSEX (ELSUB(I), ELE, ION)
C
        if((ELE.eq.OLE).and.(ION.eq.IOO)) then
          INDEX(I,1) = NT
          NT         = NT+1
          INDEX(I,2) = NT
        else
          NT         = NT+1
          INDEX(I,1) = NT
          NT         = NT+1
          INDEX(I,2) = NT
        end if
C
  100 continue
C     !END
      call BYE ('DICTATE')
C
      return
      end
