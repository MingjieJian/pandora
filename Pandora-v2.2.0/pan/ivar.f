      subroutine IVAR
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1986 Jul 24
C---- Allocates scratch storage for PRIVET.
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
C
C---- SOBOLEV     as of 1993 Aug 20
      integer     MDSOB
      common      /SOBOLEV/ MDSOB
C     Counter value for Sobolev escape probability calculation.
C     .
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('IVAR')
C     !BEG
      call WGET (IS ,CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+N
      IN( 3) = IN( 2)+N
      IN( 4) = IN( 3)+N
      IN( 5) = IN( 4)+N
      IN( 6) = IN( 5)+N
      IN( 7) = IN( 6)+6*MDSOB
      IN( 8) = IN( 7)+9*N
      IN( 9) = IN( 8)+9*N
      IN(10) = IN( 9)+N
      MUX    = IN(10)+N
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('IVAR')
C
      return
      end
