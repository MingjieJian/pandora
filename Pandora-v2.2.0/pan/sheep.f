      subroutine SHEEP
     $(X,IX,W,IW,KABS,KEMIT,KCSF,KOUT,KODE,MODE,KTRU)
C
C     Rudolf Loeser, 1986 Jul 11
C---- Does the initial setup for Continuum Calculations.
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IIADRS, IKTYPE, IN, IS, IW, IWAVES, IX, IXCBL, IXLYB,
     $        IXPBL, KABS, KCSF, KEMIT, KODE, KOUT, KTRU, MODE, MOX, NW
C     !DASH
      external LILITU, JACOB, CYBELE, WGIVE, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      dimension IN(6)
      equivalence
     $(IN( 1),IXPBL ),(IN( 2),IXLYB ),(IN( 3),IXCBL ),(IN( 4),IWAVES),
     $(IN( 5),IIADRS),(IN( 6),IKTYPE)
C
      call HI ('SHEEP')
C     !BEG
C     (Get, and allocate, W allotment)
      call LILITU (IN, IS, MOX, 'SHEEP')
C
C---- Get addresses of the desired subset (as specified by KODE) of
C     continuum data blocks
      call JACOB  (KODE, MODE, NW, W(IWAVES), W(IIADRS), W(IKTYPE))
C
C---- Compute for these data blocks
      call CYBELE (X, IX, W, IW, W(IWAVES), W(IKTYPE), W(IIADRS), NW,
     $             W(IXCBL), W(IXPBL), W(IXLYB), KABS, KEMIT, KCSF,
     $             KOUT, KODE, KTRU)
C
C     (Give back W allotment)
      call WGIVE  (W, 'SHEEP')
C     !END
      call BYE ('SHEEP')
C
      return
      end
