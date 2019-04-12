      subroutine MARILYN
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1980 Feb 19
C---- Allocates scratch storage for ARGO.
C     (This is version 2 of MARILYN.)
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, N, N2
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
      call HI ('MARILYN')
C     !BEG
      call WGET (IS ,CALLER)
C
      N2 = 2*N
C
      IN( 1) = IS
      IN( 2) = IN( 1)+N2
      IN( 3) = IN( 2)+N2
      IN( 4) = IN( 3)+N2
      IN( 5) = IN( 4)+N2
      IN( 6) = IN( 5)+N2
      MUX    = IN( 6)+N2
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('MARILYN')
C
      return
      end
