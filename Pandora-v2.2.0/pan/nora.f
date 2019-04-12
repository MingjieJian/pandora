      subroutine NORA
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1970 Feb 18
C---- Allocates scratch storage for TAURUS.
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
      call HI ('NORA')
C     !BEG
      call WGET (IS ,CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+N**2
      MUX    = IN( 2)+N
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('NORA')
C
      return
      end
