      subroutine EPSOM
     $(WVL,V,N,DV)
C
C     Rudolf Loeser, 1983 Sep 01
C---- Computes wavelength shift due to velocity.
C     (V, if needed, has already been projected onto the current ray.)
C     !DASH
      save
C     !DASH
      real*8 CON11, DV, F, V, WVL
      integer N
C     !DASH
      external RIGEL, MOVE1, CONMUL, HI, BYE
C
C               V(N), DV(N)
      dimension V(*), DV(*)
C
      call HI ('EPSOM')
C     !BEG
      call RIGEL  (11, CON11)
      F = WVL*CON11
C
      call MOVE1  (V, N, DV)
      call CONMUL (F, DV, N)
C     !END
      call BYE ('EPSOM')
C
      return
      end
