      subroutine IMRE
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1997 Sep 26
C---- Allocates scratch storage for JONQUIL.
C     (This is version 2 of IMRE.)
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
      call HI ('IMRE')
C     !BEG
      call WGET (IS ,CALLER)
C
      IN( 1) = IS
      MUX    = IN( 1)+N
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('IMRE')
C
      return
      end
