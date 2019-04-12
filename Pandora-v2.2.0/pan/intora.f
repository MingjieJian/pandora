      subroutine INTORA
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 2000 Jul 21
C---- Allocates scratch storage for ORNATI.
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
      call HI ('INTORA')
C     !BEG
      call WGET (IS ,CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+KM
      IN( 3) = IN( 2)+KM
      IN( 4) = IN( 3)+KM
      IN( 5) = IN( 4)+KM
      IN( 6) = IN( 5)+KM
      MUX    = IN( 6)+KM
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('INTORA')
C
      return
      end
