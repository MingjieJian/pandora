      subroutine PAZYP
     $(LU,CGR,YH,LHHSE,KASE)
C
C     Rudolf Loeser, 2003 Aug 12
C---- Prints header for ZAPPY.
C     !DASH
      save
C     !DASH
      real*8 CGR, YH
      integer KASE, LHHSE, LU
C     !DASH
      external LINER, ADIGE, HI, BYE
C
      call HI ('PAZYP')
C     !BEG
      if(LU.gt.0) then
        call LINER (2, LU)
        write (LU,100)
  100   format(' ','Hydrostatic Equilibrium Calculations',74X,
     $             '(Option AHSEPRNT)')
        call ADIGE (LU, CGR, YH, LHHSE, KASE)
      end if
C     !END
      call BYE ('PAZYP')
C
      return
      end
