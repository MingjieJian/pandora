      subroutine FOX
     $(NDT,F,APD,VEC,TW,SUM)
C
C     Rudolf Loeser, 1973 Oct 24
C---- Integrates, for SINEW.
C     !DASH
      save
C     !DASH
      real*8 APD, F, SUM, TW, VEC
      integer NDT
C     !DASH
      external ARRMUL, SUMPROD, HI, BYE
C
C               F(NDT), APD(NDT), TW(NDT), VEC(NDT)
      dimension F(*),   APD(*),   TW(*),   VEC(*)
C
      call HI ('FOX')
C     !BEG
      call ARRMUL  (F,APD,VEC,NDT)
      call SUMPROD (SUM,VEC,1,TW,1,NDT)
C     !END
      call BYE ('FOX')
C
      return
      end
