      subroutine LAGOON
     $(N,NT,INDEX,THETA,PE,PFO,PFE,VEC)
C
C     Rudolf Loeser, 1982 Jun 29
C---- Computes Partition Functions, for CORAL.
C     (This is version 3 of LAGOON.)
C     !DASH
      save
C     !DASH
      real*8 PE, PFE, PFO, PRTLM, THETA, VEC
      integer I, INDEX, ION, J, JO, K, N, NT
      character ELE*2
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (RZQ( 47),PRTLM)
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
C     !EJECT
      external ESSEX, HAMBURG, TANAGER, HI, BYE
C
C               INDEX(NMT,2), PFO(N,NT), PFE(N,NT), THETA(N), VEC(N),
      dimension INDEX(NMT,*), PFO(N,*),  PFE(N,*),  THETA(*), VEC(*),
C
C               PE(N)
     $          PE(*)
C
      call HI ('LAGOON')
C     !BEG
      J = 0
      do 100 K = 1,NMT
        JO = J
        I  = INDEX(K,1)
        J  = INDEX(K,2)
        call ESSEX     (ELSUB(K),ELE,ION)
C
        if(I.ne.JO) then
          call HAMBURG (ELE,ION    ,THETA,PE,PFO(1,I),VEC,N)
          call TANAGER (PFO(1,I),PFE(1,I),PRTLM,N)
        end if
C
        call HAMBURG   (ELE,(ION+1),THETA,PE,PFO(1,J),VEC,N)
        call TANAGER   (PFO(1,J),PFE(1,J),PRTLM,N)
  100 continue
C     !END
      call BYE ('LAGOON')
C
      return
      end
