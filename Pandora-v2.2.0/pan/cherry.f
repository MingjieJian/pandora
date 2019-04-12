      subroutine CHERRY
     $(XK,KK,Y,AK,F,G,XI,W,IW,KODE)
C
C     Rudolf Loeser, 1968 Aug 06
C---- Computes AK, the weights for the Lyman Source Function
C     frequency summations.
C     Returns with KODE=1 if all seems OK, =0 if not.
C     (See also MYRTLE.)
C     !DASH
      save
C     !DASH
      real*8 AK, F, G, HALF, ONE, T, THIRD, W, XI, XK, Y, ZERO
      integer IW, J, KK, KODE
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(13),THIRD )
      equivalence (DLIT(12),HALF  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external ZERO1, PLUM, SUMPROD, HI, BYE
C
      dimension W(*), IW(*)
C
C               XK(KK), AK(KK), G(KK), F(KK,KK), XI(KK)
      dimension XK(*),  AK(*),  G(*),  F(KK,*),  XI(*)
C
      call HI ('CHERRY')
C     !BEG
      call ZERO1       (AK, KK)
C
C---- Compute the inverse of the matrix of modified F-functions
      call PLUM        (XK, KK, Y, F, XI, W, IW, KODE)
C
      if(KODE.eq.1) then
C----   Compute the vector G
        G(1) = XK(KK)-ONE
C
        T = HALF*(ONE-THIRD*Y)
        do 100 J = 2,KK
          G(J) = T*(XK(J)-ONE)
  100   continue
C
C----   and now multiply to form AK
        do 101 J = 1,KK
          call SUMPROD (AK(J), G, 1, F(1,J), 1, KK)
  101   continue
      end if
C     !END
      call BYE ('CHERRY')
C
      return
      end
