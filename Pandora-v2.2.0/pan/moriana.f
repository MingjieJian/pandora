      subroutine MORIANA
     $(IN,IS,MUX,CALLER,NMAX,KMAX)
C
C     Rudolf Loeser, 1977 Sep 23
C---- Allocates scratch storage for FALAKI.
C     (This is version 2 of MORIANA.)
C     !DASH
      save
C     !DASH
      integer IN, IS, KMAX, MUX, N, NKX, NMAX
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
      call HI ('MORIANA')
C     !BEG
      call WGET (IS,  CALLER)
C
      NKX = NMAX*KMAX
C
      IN( 1) = IS
      IN( 2) = IN( 1)+NMAX
      IN( 3) = IN( 2)+KMAX
      IN( 4) = IN( 3)+NKX
      IN( 5) = IN( 4)+N*KMAX
      MUX    = IN( 5)+NKX
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('MORIANA')
C
      return
      end
