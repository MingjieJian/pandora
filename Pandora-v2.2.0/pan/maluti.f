      subroutine MALUTI
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1983 Mar 03
C---- Allocates scratch storage for BASUTO.
C     (This is version 2 of MALUTI.)
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
      call HI ('MALUTI')
C     !BEG
      call WGET (IS,  CALLER)
C
      NKM = N*KM
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+N
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
      call BYE ('MALUTI')
C
      return
      end
