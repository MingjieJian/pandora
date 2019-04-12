      subroutine MALOSE
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 2001 Dec 12
C---- Allocates scratch storage for HYDRANT.
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
      call HI ('MALOSE')
C     !BEG
      call WGET (IS ,CALLER)
C
      IN( 1) = IS
      MUX    = IN( 1)+N
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('MALOSE')
C
      return
      end
