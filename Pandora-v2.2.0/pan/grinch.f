      subroutine GRINCH
     $(L,I,N,K,DL,DDL,FDDL,DW,DP,V,W,DUMP,GII,TIME)
C
C     Rudolf Loeser, 2004 Nov 18
C---- Computes an array of GII values, for GAFFE.
C     V and W are scratch arrays.
C     !DASH
      save
C     !DASH
      real*8 A, DDL, DL, DP, DW, FDDL, GII, OFF, ONE, R, TIME, TIN,
     $       TOUT, V, VP, W, ZERO
      integer I, J, JP, K, L, N
      logical DUMP
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external SECOND, DIVIDE, GITTAR, HI, BYE
C
C               DP(N,LDL), DDL(LDL), FDDL(N), DL(K), DW(N), V(K), W(K),
      dimension DP(N,*),   DDL(*),   FDDL(*), DL(*), DW(*), V(*), W(*),
C
C               GII(K,K)
     $          GII(K,*)
C
      call HI ('GRINCH')
C     !BEG
      call SECOND   (TIN)
C
      call DIVIDE   (ONE, DW(I), R)
      A   = DP(I,L)*R
      OFF = DDL(L)*FDDL(I)
      do 101 J = 1,K
        VP = (DL(J)-OFF)*R
        do 100 JP = 1,K
          V(JP) = (DL(JP)-OFF)*R
  100   continue
        call GITTAR (K, A, V, VP, W, GII(1,J), DUMP, J)
  101 continue
C
      call SECOND   (TOUT)
      TIME = TOUT-TIN
C     !END
      call BYE ('GRINCH')
C
      return
      end
