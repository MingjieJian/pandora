      subroutine SWAT
     $(XI,K,Y,A,W,IW,KSYM,KODE)
C
C     Rudolf Loeser, 2006 May 24
C---- Computes frequency integration weights for LSF calculation.
C
C     Returns KSYM = 0 if this is a full profile, = 1 if symmetric.
C
C     Returns KODE = 0 if the calculation failed, = 1 if OK.
C
C     (This is version 2 of SWAT.)
C     !DASH
      save
C     !DASH
      real*8 A, TWO, W, XI, Y
      integer IAB, IAR, IN, IS, IW, IXIB, IXIR, K, KB, KODE, KODEB,
     $        KODER, KR, KSYM, MOX
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 3),TWO   )
C     !DASH
C     !EJECT
      external NEGATE, QUEBEC, KOWRI, CONMUL, MOVE1, REVERSD, HAPAX,
     $         ZERO1, MONEY, WGIVE, HI, BYE
C
      dimension W(*), IW(*)
C
C               XI(K), A(K)
      dimension XI(*), A(*)
C
      dimension IN(4)
      equivalence
     $(IN( 1),IXIR  ),(IN( 2),IAR   ),(IN( 3),IXIB  ),(IN( 4),IAB   )
C
      call HI ('SWAT')
C     !BEG
C     (Get, and allocate, W allotment)
      call MONEY     (IN, IS, MOX, 'SWAT')
C
      call ZERO1     (A, K)
C
      call QUEBEC    (XI, K, 'XI', 'COWRY', KB)
      if(KB.le.1) then
C----   Symmetric half profile
        call KOWRI   (XI, K, Y, A, W, IW, KODE)
        call CONMUL  (TWO, A, K)
        KSYM = 1
      else
C----   Full profile
        KR = K-KB+1
        call MOVE1   (XI(KB), KR, W(IXIR))
        call KOWRI   (W(IXIR), KR, Y, W(IAR), W, IW, KODER)
C
        call MOVE1   (XI, KB, W(IXIB))
        call REVERSD (W(IXIB), 1, KB)
        call NEGATE  (W(IXIB), KB)
        call KOWRI   (W(IXIB), KB, Y, W(IAB), W, IW, KODEB)
C
        call HAPAX   (W(IAB), KB, W(IAR), KR, A, K, 2)
        KODE = KODER*KODEB
        KSYM = 0
      end if
C
C     (Give back W allotment)
      call WGIVE     (W, 'SWAT')
C     !END
      call BYE ('SWAT')
C
      return
      end
