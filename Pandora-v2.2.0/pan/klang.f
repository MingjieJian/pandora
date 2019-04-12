      subroutine KLANG
     $(IN,IS,MUX,CALLER,NEW)
C
C     Rudolf Loeser, 1997 Dec 18
C---- Allocates scratch storage for VORPAL.
C     (This is version 2 of KLANG.)
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, N, NEW, NN
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('KLANG')
C     !BEG
      call WGET (IS ,CALLER)
C
      NN = N*NEW
C
      IN( 1) = IS
      IN( 2) = IN( 1)+NN
      IN( 3) = IN( 2)+NN
      IN( 4) = IN( 3)+NN
      IN( 5) = IN( 4)+NN
      MUX    = IN( 5)+NN
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('KLANG')
C
      return
      end
