      subroutine MORGAN
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1983 Dec 07
C---- Allocates scratch storage for RIANNON.
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
      call HI ('MORGAN')
C     !BEG
      call WGET (IS ,CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+N
      MUX    = IN( 2)+N
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('MORGAN')
C
      return
      end
