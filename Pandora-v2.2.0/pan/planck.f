      subroutine PLANCK
     $(FNU,ALF,TE,BETA,B)
C
C     Rudolf Loeser, 1981 Jul 27.
C---- Computes a single value of the Planck function.
C     (This is version 3 of PLANCK.)
C     !DASH
      save
C     !DASH
      real*8 ALF, B, BETA, E, F, FNU, R, TE, W
C     !DASH
      external HUNK, QEXP1, DIVIDE, HI, BYE
C
      call HI ('PLANCK')
C     !BEG
      E = BETA
      call HUNK   (TE, FNU, 1, W)
      call QEXP1  (W, E, 1, F)
      call DIVIDE (E, F, R)
C
      B = R*ALF
C     !END
      call BYE ('PLANCK')
C
      return
      end
