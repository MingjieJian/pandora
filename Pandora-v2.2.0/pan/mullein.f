      subroutine MULLEIN
     $(LIMP,NLH,A,B,E,F)
C
C     Rudolf Loeser, 1981 May 29
C---- Computes intermediates, for MALLOW.
C     !DASH
      save
C     !DASH
      real*8 A, B, E, F, ONE, OXI, OXN, OXT, W, XI, XN, XT
      integer I, LIMP, NLH
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
C               A(LIMP), B(LIMP), E(LIMP), F(LIMP)
      dimension A(*),    B(*),    E(*),    F(*)
C
C
      call HI ('MULLEIN')
C     !BEG
      XT  = LIMP**2
      OXT = ONE/XT
C
      XN  = NLH**2
      OXN = ONE/XN
C
      do 100 I = (NLH+1),(LIMP-1)
        XI  = I**2
        OXI = ONE/XI
        W   = (ONE-XN/XI)/(ONE-XN/XT)
C
        A(I) = (ONE-W)*(XI/XN)
        B(I) = W*(XI/XT)
        E(I) = OXN-OXI
        F(I) = OXT-OXI
  100 continue
C     !END
      call BYE ('MULLEIN')
C
      return
      end
