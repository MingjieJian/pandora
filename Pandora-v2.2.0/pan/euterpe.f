      subroutine EUTERPE
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1991 Jan 03
C---- Allocates scratch storage for ERATO.
C     (This is version 2 of EUTERPE.)
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
      call HI ('EUTERPE')
C     !BEG
      call WGET (IS ,CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+N
      IN( 3) = IN( 2)+N
      IN( 4) = IN( 3)+N
      MUX    = IN( 4)+20*N
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('EUTERPE')
C
      return
      end
