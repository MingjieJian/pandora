      subroutine GABBRO
     $(TRD,TVW,TSK,TRS,TIB,F,DPM,C,DP)
C
C     Rudolf Loeser, 1980 Aug 14
C---- Computes damping parameter, and its components.
C     !DASH
      save
C     !DASH
      real*8 C, DP, DPM, F, TIB, TRD, TRS, TSK, TVW
C     !DASH
      external HI, BYE
C
C               F(5), C(5)
      dimension F(*), C(*)
C
      call HI ('GABBRO')
C     !BEG
      C(1) = F(1)*TRD*DPM
      C(2) = F(2)*TVW*DPM
      C(3) = F(3)*TSK*DPM
      C(4) = F(4)*TRS*DPM
      C(5) = F(5)*TIB*DPM
C
      DP = C(1)+C(2)+C(3)+C(4)+C(5)
C     !END
      call BYE ('GABBRO')
C
      return
      end
