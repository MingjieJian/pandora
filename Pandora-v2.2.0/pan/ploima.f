      subroutine PLOIMA
     $(N,Z,TE,ROSSK,TEFF,W)
C
C     Rudolf Loeser, 1987 Mar 23
C---- Computes effective temperature.
C     !DASH
      save
C     !DASH
      real*8 ROSSK, TE, TEFF, W, Z
      integer IDTE4, IN, IS, ITE4, MOX, N
C     !DASH
      external NAURU, TITI, WGIVE, HI, BYE
C
      dimension W(*)
C
C               Z(N), TE(N), ROSSK(N), TEFF(N)
      dimension Z(*), TE(*), ROSSK(*), TEFF(*)
C
      dimension IN(2)
      equivalence
     $(IN( 1),ITE4  ),(IN( 2),IDTE4 )
C
      call HI ('PLOIMA')
C     !BEG
C     (Get, and allocate, W allotment)
      call NAURU (IN,IS,MOX,'PLOIMA')
C
      call TITI  (N,Z,TE,W(ITE4),W(IDTE4),ROSSK,TEFF)
C
C     (Give back W allotment)
      call WGIVE (W,'PLOIMA')
C     !END
      call BYE ('PLOIMA')
C
      return
      end
