      subroutine MOOLA
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1997 Apr 25
C---- Allocates integer scratch storage for LIME.
C     !DASH
      save
C     !DASH
      integer IN, IS, K, KK, KKX, MUX, N, NSL
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(10),KK )
      equivalence (JZQ(40),NSL)
      equivalence (JZQ(12),KKX)
C     !DASH
      external  IGET, ILCK, HI, BYE
      intrinsic max
C
      dimension IN(*)
C
      call HI ('MOOLA')
C     !BEG
      call IGET (IS,  CALLER)
C
      K = max(KK,KKX)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+K
      IN( 3) = IN( 2)+N
      IN( 4) = IN( 3)+NSL
      IN( 5) = IN( 4)+NSL
      MUX    = IN( 5)+KKX
C
      call ILCK (MUX, CALLER)
C     !END
      call BYE ('MOOLA')
C
      return
      end
