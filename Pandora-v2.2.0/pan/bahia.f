      subroutine BAHIA
     $(NLN,WSLN,NFL,WNRMLA,FNRMLA,WNRMLB,FNRMLB)
C
C     Rudolf Loeser, 2005 Nov 03
C---- Set up data for the simulated background H Ly lines.
C     (This is version 2 of BAHIA.)
C
C     Remember that NFL is computed in DEFAULT !
C     !DASH
      save
C     !DASH
      real*8 FNRMLA, FNRMLB, HALF, WLA, WLB, WNRMLA, WNRMLB, WSLN
      integer I, J, K, NFL, NLN
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(12),HALF  )
C     !DASH
      external ONE1, HI, BYE
C
C               WNRMLA(NFL), FNRMLA(NFL), WNRMLB(NFL), FNRMLB(NFL),
      dimension WNRMLA(*),   FNRMLA(*),   WNRMLB(*),   FNRMLB(*),
C
C               WSLN(NLN)
     $          WSLN(*)
C
      data WLA,WLB /1215.67D0, 1025.72D0/
C
      call HI ('BAHIA')
C     !BEG
      WNRMLA(NLN) = WLA
      WNRMLB(NLN) = WLB
C
      I = NLN
      K = NLN
      do 100 J = 2,NLN
        I = I-1
        WNRMLA(I) = WLA -     WSLN(J)
        WNRMLB(I) = WLB -HALF*WSLN(J)
        K = K+1
        WNRMLA(K) = WLA +     WSLN(J)
        WNRMLB(K) = WLB +HALF*WSLN(J)
  100 continue
C
      call ONE1 (FNRMLA, NFL)
      call ONE1 (FNRMLB, NFL)
C     !END
      call BYE ('BAHIA')
C
      return
      end
