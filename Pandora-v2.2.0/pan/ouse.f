      subroutine OUSE
     $(KODE,MODE,KTRU,KABS,KEMIT,KCSF,KOUT,X,IX,W,IW)
C
C     Rudolf Loeser, 1980 Feb 04
C---- Background (continuum) Calculations:
C
C---- KODE = 3: current line (PRD, FDB)
C          = 2: Lyman
C          = 1: everything else
C
C---- KTRU = 0: regular background (continuum)
C          = 1: "line-free" background
C
C     KABS = 1: compute Absorption,                = 0: don't;
C     KEMIT= 1: compute Emission,                  = 0: don't;
C     KCSF = 1: compute Continuum Source Function, = 0: don't;
C     KOUT = 1: give formatted output (printout),  = 0: don't.
C
C---- Note: KEMIT=1 while KABS=0 makes no sense!
C     Note: MODE is used only with KODE=3.
C
C     (This is version 2 of OUSE.)
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IW, IX, KABS, KCSF, KEMIT, KODE, KOUT, KTRU, MODE, NPROG
C     !DASH
      external KNIGHT, LOGIN, SHEEP, LOGOUT, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      data NPROG /7/
C
      call HI ('OUSE')
C     !BEG
C---- Check control parameters --- abort if not OK.
      call KNIGHT (KODE, KABS, KEMIT)
C
      call LOGIN  (NPROG)
      call SHEEP  (X, IX, W, IW, KABS, KEMIT, KCSF, KOUT,
     $             KODE, MODE, KTRU)
      call LOGOUT (NPROG)
C     !END
      call BYE ('OUSE')
C
      return
      end
