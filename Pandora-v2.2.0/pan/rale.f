      subroutine RALE
     $(N,NL,KK,RLI,CP,AK,GK,XK,EMUX,XJIK,RLO,KOLEV,YK,XNU)
C
C     Rudolf Loeser, 1974 Dec 26
C---- Computes RL, for ROPE.
C     !DASH
      save
C     !DASH
      real*8 AK, CP, EMUX, GK, RLI, RLO, XJIK, XK, XNU, YK, ZERO
      integer KK, KOLEV, N, NL
      logical KOOL
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external MOVE1, MALE, YAK, HI, BYE
C
C               RLI(N,NL), YK(NL), RLO(N), CP(NSL+1), AK(KKX), XK(KKX),
      dimension RLI(N,*),  YK(*),  RLO(*), CP(*),     AK(*),   XK(*),
C
C               GK(KKX), XJIK(N,KKX), EMUX(N,KKX), XNU(NSL)
     $          GK(*),   XJIK(*),     EMUX(*),     XNU(*)
C
      data KOOL /.false./
C
      call HI ('RALE')
C     !BEG
C---- Save previous RL
      call MOVE1 (RLI(1,KOLEV), N, RLO)
C---- Compute new RL
      call MALE  (CP, RLI(1,KOLEV), KK, AK, GK, XK, XJIK, N, KOLEV,
     $            EMUX, KOOL, XNU)
C---- Provide for additional recombination
      if(YK(KOLEV).ne.ZERO) then
        call YAK (N, YK(KOLEV), RLI(1,KOLEV))
      end if
C     !END
      call BYE ('RALE')
C
      return
      end
