      subroutine ILDIR
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1992 Mar 23
C---- Allocates scratch storage for PLOVER.
C     !DASH
      save
C     !DASH
      integer IN, IS, KM, MUX, N, NFL, NKM
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
      external  WGET, WLCK, HI, BYE
      intrinsic max
C
      dimension IN(*)
C
      call HI ('ILDIR')
C     !BEG
      call WGET (IS,  CALLER)
C
      NFL = max(N,KM)
      NKM = N*KM
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+NKM
      IN( 3) = IN( 2)+N
      IN( 4) = IN( 3)+NFL
      IN( 5) = IN( 4)+N
      IN( 6) = IN( 5)+N
      MUX    = IN( 6)+NKM
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('ILDIR')
C
      return
      end
