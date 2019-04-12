      subroutine PLUM
     $(XK,KK,Y,F,XI,W,IW,KODE)
C
C     Rudolf Loeser, 1968 Aug 06
C---- Computes the inverse F-matrix for CHERRY.
C     Returns with KODE=1 if all seems ok, =0 if not.
C     !DASH
      save
C     !DASH
      real*8 F, ONE, W, XI, XK, Y
      integer IW, KK, KODE
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external MOVE1, CONSUB, MADRONE, HI, BYE
C
      dimension W(*), IW(*)
C
C               XK(KK), XI(KK), F(KK,KK)
      dimension XK(*),  XI(*),  F(*)
C
      call HI ('PLUM')
C     !BEG
      call MOVE1   (XK, KK, XI)
      call CONSUB  (ONE, XI, KK)
C
      call MADRONE (XI, KK, Y, F, W, IW, KODE)
C     !END
      call BYE ('PLUM')
C
      return
      end
