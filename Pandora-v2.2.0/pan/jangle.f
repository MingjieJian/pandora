      subroutine JANGLE
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1975 Jul 30
C---- Allocates scratch storage for CRATON.
C     !DASH
      save
C     !DASH
      integer IN, IS, IW, MUX, N, NL
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
C     !DASH
      external  WGET, WLCK, HI, BYE
      dimension IN(*)
C
      call HI ('JANGLE')
C     !BEG
      call WGET (IS ,CALLER)
C
      IW = N*(NL+1)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+IW
      MUX    = IN( 2)+N
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('JANGLE')
C
      return
      end
