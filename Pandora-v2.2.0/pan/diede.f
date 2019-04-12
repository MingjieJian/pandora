      subroutine DIEDE
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1998 Oct 01
C---- Allocates scratch storage for EDDIE.
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
      call HI ('DIEDE')
C     !BEG
      call WGET (IS ,CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+N
      IN( 3) = IN( 2)+N
      IN( 4) = IN( 3)+N
      IN( 5) = IN( 4)+N
      MUX    = IN( 5)+N
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('DIEDE')
C
      return
      end
