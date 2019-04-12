      subroutine ROBBER
     $(INDEX,N,TE,PF,CFH)
C
C     Rudolf Loeser, 2004 Sep 20
C---- Computes CFH (Hydrogen) for lower-level charge exchange.
C     !DASH
      save
C     !DASH
      real*8 ARG, CFH, CSP, DNU, EXT, FAC, PF, TE, TWO, XNK, XNUKH,
     $       dummy
      integer I, INDEX, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 3),TWO   )
C     !DASH
      external RASH, PROD, HI, BYE
C
C               TE(N), PF(N), CFH(N)
      dimension TE(*), PF(*), CFH(*)
C
      data XNUKH /3.288068D0/
C
      call HI ('ROBBER')
C     !BEG
      call RASH   (1, INDEX, dummy, dummy, dummy, dummy, CSP, XNK)
C
      DNU = XNK-XNUKH
      FAC = TWO/CSP
      do 100 I = 1,N
        call PROD (TE(I), DNU, 1, ARG, EXT)
        CFH(I) =(FAC*PF(I))*EXT
  100 continue
C     !END
      call BYE ('ROBBER')
C
      return
      end
