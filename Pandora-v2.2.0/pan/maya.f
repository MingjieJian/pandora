      subroutine MAYA
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1973 Jul 24
C---- Allocates scratch storage for DEVI.
C     !DASH
      save
C     !DASH
      integer IN, IS, KNW, MUX, N
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(25),KNW)
C
C---- KNTKUPL     as of 1993 Sep 15
      integer     KNTKU
      parameter   (KNTKU=5)
C     Control parameter for "Line" opacity plots.
C     (Used in CADMOS, MAYA, SILAS.)
C     .
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('MAYA')
C     !BEG
      call WGET (IS ,CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+10
      IN( 3) = IN( 2)+990
      IN( 4) = IN( 3)+N
      IN( 5) = IN( 4)+KNW*KNTKU
      MUX    = IN( 5)+N
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('MAYA')
C
      return
      end
