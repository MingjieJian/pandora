      subroutine STONY
     $(NO,LLEFT,LINEL,ML,WRAL,RNUL,RCPL,YRL,LRITE,LINER,MR,WRAR,RNUR,
     $ RCPR,YRR)
C
C     Rudolf Loeser, 1980 Mar 05
C---- Controls printing, for PEACH.
C     (This is version 2 of STONY.)
C     !DASH
      save
C     !DASH
      real*8 RCPL, RCPR, RNUL, RNUR, WRAL, WRAR, YRL, YRR
      integer I, LLEFT, LRITE, ML, MR, NO
      character LINEL*60, LINER*60
C     !DASH
      external  MUDDY, HI, BYE
      intrinsic max
C
C               RNUL(ML), RCPL(ML), RNUR(MR), RCPR(MR), WRAR(MR),
      dimension RNUL(*),  RCPL(*),  RNUR(*),  RCPR(*),  WRAR(*),
C
C               YRR(MR), WRAL(ML), YRL(ML)
     $          YRR(*),  WRAL(*),  YRL(*)
C
      call HI ('STONY')
C     !BEG
      do 101 I = 1,(max(ML,MR))
C
        call MUDDY (I, ML, LLEFT, WRAL(I), RNUL(I), RCPL(I), YRL(I),
     $              LINEL)
        call MUDDY (I, MR, LRITE, WRAR(I), RNUR(I), RCPR(I), YRR(I),
     $              LINER)
C
        write (NO,100) LINEL,LINER
  100   format(' ',2A60)
  101 continue
C     !END
      call BYE ('STONY')
C
      return
      end
