      subroutine IRFON
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1982 Feb 24
C---- Allocates scratch storage for DYLAN.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, N, NL, NNT
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('IRFON')
C     !BEG
      call WGET (IS ,CALLER)
C
      NNT = N*(((NL-1)*NL)/2)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+NNT
      IN( 3) = IN( 2)+N*NL
      MUX    = IN( 3)+N
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('IRFON')
C
      return
      end
