      subroutine LANYARD
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1980 Feb 07
C---- Allocates scratch storage for QUEST.
C     (This is version 2 of LANYARD.)
C     !DASH
      save
C     !DASH
      integer IN, IS, KM, MUX, N, NKM
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(49),KM )
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('LANYARD')
C     !BEG
      call WGET (IS, CALLER)
C
      NKM = N*KM
C
      IN( 1) = IS
      IN( 2) = IN( 1)+NKM
      IN( 3) = IN( 2)+NKM
      IN( 4) = IN( 3)+NKM
      IN( 5) = IN( 4)+NKM
      IN( 6) = IN( 5)+NKM
      IN( 7) = IN( 6)+NKM
      IN( 8) = IN( 7)+NKM
      IN( 9) = IN( 8)+NKM
      IN(10) = IN( 9)+NKM
      IN(11) = IN(10)+NKM
C
      MUX    = IN(11)+NKM
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('LANYARD')
C
      return
      end
