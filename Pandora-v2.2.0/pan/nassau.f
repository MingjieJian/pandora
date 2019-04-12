      subroutine NASSAU
     $(N,NMTS,FMC,FMCS,ETA,ETAS,AMUX,IMUX,IPNT,IWRK)
C
C     Rudolf Loeser, 1982 Jun 07
C---- Selects subsets, for POCKET.
C     !DASH
      save
C     !DASH
      real*8 AMUX, ETA, ETAS, FMC, FMCS
      integer IMUX, IPNT, IWRK, N, NMTS
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
      external FERIA, EGAS, ORRY, DORIA, HI, BYE
C
C               AMUX(NMT), IMUX(NMT), IPNT(NMT), IWRK(NMT), FMC(N,NMT),
      dimension AMUX(*),   IMUX(*),   IPNT(*),   IWRK(*),   FMC(*),
C
C               FMCS(N,NMT), ETA(N,NMT), ETAS(N,NMT)
     $          FMCS(*),     ETA(*),     ETAS(*)
C     !EJECT
C
      call HI ('NASSAU')
C     !BEG
C---- Get sorted list of element indices
      call FERIA   (N,FMC,AMUX,IMUX,IPNT,IWRK)
C---- Get NMTS, number of elements in subset
      call EGAS    (AMUX,NMT,NMTS)
C
      if(NMTS.gt.0) then
C----   Set up subset of element symbols
        call ORRY  (NMTS,IMUX)
C----   Set up subset of FMC
        call DORIA (N,NMT,FMC,NMTS,FMCS,IMUX)
C----   Set up subset of ETA
        call DORIA (N,NMT,ETA,NMTS,ETAS,IMUX)
      end if
C
C     !END
      call BYE ('NASSAU')
C
      return
      end
