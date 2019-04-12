      subroutine HOOP
     $(Z,THETA,PE,H)
C
C     Rudolf Loeser, 1982 Jun 09
C---- Computes H for the calculation of Partition Functions.
C     (For reference, see listing of "DEPART".)
C     !DASH
      save
C     !DASH
      real*8 C, FAC, H, H2, HP, PE, RH6, SIXTH, THETA, Z, ZDCHI
      integer N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(16),SIXTH )
C     !DASH
      external  CHIDE, HI, BYE
      intrinsic min, max
C
      dimension C(4)
C
      data C   /1.927D-10, 1.255D-11, 2.644D-12, 8.92D-13/
      data FAC /1.3595D1/
C
      call HI ('HOOP')
C     !BEG
      call CHIDE  (Z, THETA, PE, ZDCHI)
      H2 = (FAC*(Z**2))/ZDCHI
      H  = sqrt(H2)
C
      N = Z
      N = max(min(N,4),1)
      RH6 = THETA*PE*C(N)
      HP  = RH6**(-SIXTH)
C
      H = min(H,HP)
C     !END
      call BYE ('HOOP')
C
      return
      end
