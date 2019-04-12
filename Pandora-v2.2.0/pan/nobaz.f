      subroutine NOBAZ
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 2000 Jun 29
C---- Allocates scratch storage for BOZENIA.
C     !DASH
      save
C     !DASH
      integer IN, IS, KM, MUX
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(49),KM )
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('NOBAZ')
C     !BEG
      call WGET (IS ,CALLER)
C
      IN( 1) = IS
      MUX    = IN( 1)+KM
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('NOBAZ')
C
      return
      end
