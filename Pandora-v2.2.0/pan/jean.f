      subroutine JEAN
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1975 Sep 26
C---- Allocates scratch storage for HELL.
C     !DASH
      save
C     !DASH
      integer IN, IS, KKX, MUX, N, NKK
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(12),KKX)
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('JEAN')
C     !BEG
      call WGET (IS,  CALLER)
C
      NKK = N*KKX
C
      IN( 1) = IS
      IN( 2) = IN( 1)+NKK
      MUX    = IN( 2)+NKK
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('JEAN')
C
      return
      end
