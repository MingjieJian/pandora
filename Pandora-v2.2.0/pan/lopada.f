      subroutine LOPADA
     $(NE,F1,EP1,EP2,RNDT,XLF,P,S)
C
C     Rudolf Loeser, 2001 Dec 28
C---- Computes S, for PALDAO.
C     !DASH
      save
C     !DASH
      real*8 EP1, EP2, F1, ONE, P, RAT, RNDT, S, XLF
      integer I, J, NE
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
C               P(NE,NE), EP1(NE), EP2(NE), F1(NE), RNDT(NE), XLF(NE),
      dimension P(NE,*),  EP1(*),  EP2(*),  F1(*),  RNDT(*),  XLF(*),
C
C               S(NE)
     $          S(*)
C
      call HI ('LOPADA')
C     !BEG
      call ZERO1 (S,NE)
      do 101 J = 1,NE
        RAT = (XLF(J)+F1(J)*(EP2(J)+RNDT(J)))/(ONE+EP1(J))
        do 100 I = 1,NE
          S(I) = S(I)+P(I,J)*RAT
  100   continue
  101 continue
C     !END
      call BYE ('LOPADA')
C
      return
      end
