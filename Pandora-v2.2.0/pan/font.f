      subroutine FONT
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1994 May 17
C---- Initial computation of PREF and Z-from-TAUKIN.
C     (This is version 4 of FONT.)
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IIMG, IOVER, ISWA, IW, IWS, IX, JN, LU, MO, MUX, NO
      logical RECZ
C     !COM
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST( 2),IOVER)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
      equivalence (LUNITS( 8),MO   )
C     !DASH
      external FOOL, POUT, FUZZY, IGIVE, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      dimension JN(2)
      equivalence
     $(JN( 1),ISWA  ),(JN( 2),IIMG  )
C     !EJECT
C
      call HI ('FONT')
C     !BEG
      if(IOVER.le.1) then
        LU = NO
      else
        LU = MO
      end if
C
C     (Get, and allocate, IW allotment)
      call FUZZY (JN, IWS, MUX, 'FONT')
C
C---- Compute Z, if needed, and set RECZ accordingly
      call FOOL  (X, W, IW, IW(ISWA), IW(IIMG), LU, RECZ)
C---- Recompute Z-dependent quantities, if needed
      call POUT  (X, W, IW, RECZ)
C
C     (Give back IW allotment)
      call IGIVE (IW, 'FONT')
C     !END
      call BYE ('FONT')
C
      return
      end
