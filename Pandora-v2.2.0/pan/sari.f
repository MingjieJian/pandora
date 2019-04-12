      subroutine SARI
     $(V,VR,EMU,IVSW,V2)
C
C     Rudolf Loeser, 1992 Apr 12
C---- Computes the square of the projected velocity.
C
C     If IVSW = 0, then V is isotropic broadening velocity (and values
C     of VR and EMU are not used).
C
C     If IVSW = 1, then V is tangential broadening velocity, VR is
C     radial broadening velocity, and EMU is cosine of look-angle for
C     current direction (i.e. at this point along the current ray).
C
C     (This is version 3 of SARI.)
C     !DASH
      save
C     !DASH
      real*8 E2, EMU, ONE, R2, T2, V, V2, VR
      integer IVSW
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external HI, BYE
C
      call HI ('SARI')
C     !BEG
      if(IVSW.gt.0) then
        E2 = EMU**2
        R2 = (VR**2)*(    E2)
        T2 = ( V**2)*(ONE-E2)
        V2 = R2+T2
C
      else
        V2 = ( V**2)
      end if
C     !END
      call BYE ('SARI')
C
      return
      end
