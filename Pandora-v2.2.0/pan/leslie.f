      subroutine LESLIE
     $(N,NL,XNUK,XNU,XND,CKI,BDI,CHL)
C
C     Rudolf Loeser, 1982 Feb 24
C---- Computes heating rates for bound-free transitions.
C     !DASH
      save
C     !DASH
      real*8 BDI, CHL, CKI, FAC, ONE, P, R, XND, XNU, XNUK
      integer I, J, N, NL
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external RIGEL, DIVIDE, HI, BYE
C
C               XNU(NL), XND(N,NL), CKI(N,NL), BDI(N,NL), CHL(N,NL)
      dimension XNU(*),  XND(N,*),  CKI(N,*),  BDI(N,*),  CHL(N,*)
C
      call HI ('LESLIE')
C     !BEG
      call RIGEL      (13,FAC)
C
      do 101 J = 1,NL
        P = FAC*(XNUK-XNU(J))
        do 100 I = 1,N
          call DIVIDE (ONE,BDI(I,J),R)
          CHL(I,J) = P*XND(I,J)*CKI(I,J)*(ONE-R)
  100   continue
  101 continue
C     !END
      call BYE ('LESLIE')
C
      return
      end
