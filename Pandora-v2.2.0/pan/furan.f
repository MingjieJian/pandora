      subroutine FURAN
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1989 Sep 15
C---- Allocates scratch storage for FAITH.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, N
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
      call HI ('FURAN')
C     !BEG
      call WGET (IS ,CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+N
      IN( 3) = IN( 2)+20*N
      MUX    = IN( 3)+N
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('FURAN')
C
      return
      end
