      subroutine MORA
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1983 Sep 02
C---- Allocates scratch storage for SVEIN.
C     !DASH
      save
C     !DASH
      integer IN, IS, KM, MUX, N
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(49),KM )
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('MORA')
C     !BEG
      call WGET (IS ,CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+N
      IN( 3) = IN( 2)+N
      IN( 4) = IN( 3)+N
      IN( 5) = IN( 4)+N
      IN( 6) = IN( 5)+KM
      IN( 7) = IN( 6)+N*KM
      MUX    = IN( 7)+N
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('MORA')
C
      return
      end
