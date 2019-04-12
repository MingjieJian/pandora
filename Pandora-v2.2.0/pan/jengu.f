      subroutine JENGU
     $(L,B)
C
C     Rudolf Loeser, 1990 Oct 11
C---- Computes b, for the Johnson model of the Hydrogen atom.
C     !DASH
      save
C     !DASH
      real*8 B, B1, C0, C1, C2, C3, EL2, EL3, ELL
      integer L
C     !DASH
      external HI, BYE
C
      data B1 / -6.03D-1 /
      data C0,C1,C2,C3 / 4.D0, -1.863D1, 3.624D1, -2.809D1 /
C
      call HI ('JENGU')
C     !BEG
      if(L.eq.1) then
        B = B1
C
      else
        ELL = L
        EL2 = L**2
        EL3 = EL2*ELL
C
        B = (C0+C1/ELL+C2/EL2+C3/EL3)/ELL
      end if
C     !END
      call BYE ('JENGU')
C
      return
      end
