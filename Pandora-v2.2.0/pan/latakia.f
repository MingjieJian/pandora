      subroutine LATAKIA
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1995 Aug 10
C---- Allocates integer scratch storage for SALLY.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, N, NSL
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(40),NSL)
C     !DASH
      external IGET, ILCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('LATAKIA')
C     !BEG
      call IGET (IS ,CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+NSL
      IN( 3) = IN( 2)+NSL
      MUX    = IN( 3)+N
C
      call ILCK (MUX,CALLER)
C     !END
      call BYE ('LATAKIA')
C
      return
      end
