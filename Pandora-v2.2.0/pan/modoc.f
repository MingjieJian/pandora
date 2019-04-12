      subroutine MODOC
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1987 Nov 05
C---- Allocates scratch storage for CASSIA.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, N, NE
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- KNTLYEP     as of 1988 Feb 10
      integer     KNTLE
      parameter   (KNTLE=4)
C     The number of alternative methods for computing Lyman EPn.
C     (Used in CASSIA, MODOC.)
C     .
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('MODOC')
C     !BEG
      call WGET (IS ,CALLER)
C
      NE = N*KNTLE
C
      IN( 1) = IS
      IN( 2) = IN( 1)+NE
      MUX    = IN( 2)+NE
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('MODOC')
C
      return
      end
