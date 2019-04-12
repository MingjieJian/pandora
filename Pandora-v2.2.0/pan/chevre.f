      subroutine CHEVRE
     $(N,EMU,VEX)
C
C     Rudolf Loeser, 2004 Apr 15
C---- Input for the Continuum Calculations: for the time being,
C     cosine of direction, EMU = 1.0
C     flow velocity,       VXS = 0.0
C     !DASH
      save
C     !DASH
      real*8 EMU, ONE, VEX
      integer N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external ZERO1, HI, BYE
C
C               VEX(N)
      dimension VEX(*)
C
      call HI ('CHEVRE')
C     !BEG
      EMU = ONE
      call ZERO1 (VEX, N)
C     !END
      call BYE ('CHEVRE')
C
      return
      end
