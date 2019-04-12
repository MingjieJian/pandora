      subroutine LEADER
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1975 Sep 29
C---- Allocates scratch storage for KNOT.
C     (This is version 2 of LEADER.)
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, N, NCR, NNCR
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(32),NCR)
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('LEADER')
C     !BEG
      call WGET (IS,  CALLER)
C
      NNCR = N*NCR
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+N
      IN( 3) = IN( 2)+NCR
      IN( 4) = IN( 3)+NCR
      IN( 5) = IN( 4)+NNCR
      MUX    = IN( 5)+NNCR
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('LEADER')
C
      return
      end
