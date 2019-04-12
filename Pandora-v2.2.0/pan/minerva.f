      subroutine MINERVA
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1981 Sep 08
C---- Allocates scratch storage for BELLONA.
C     !DASH
      save
C     !DASH
      integer IN, IS, MRR, MUX, N
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(15),MRR)
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('MINERVA')
C     !BEG
      call WGET (IS ,CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+N+1
      IN( 3) = IN( 2)+MRR
      MUX    = IN( 3)+MRR
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('MINERVA')
C
      return
      end
