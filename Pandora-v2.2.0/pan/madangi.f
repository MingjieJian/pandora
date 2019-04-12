      subroutine MADANGI
     $(IN,IS,MUX,CALLER,N)
C
C     Rudolf Loeser, 1989 Dec 06
C---- Allocates scratch storage for MAPPING.
C     !DASH
      save
C     !DASH
      integer IN, IS, M, MUX, N
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 3),M  )
C     !DASH
      external  WGET, WLCK, HI, BYE
      intrinsic max
C
      dimension IN(*)
C
      call HI ('MADANGI')
C     !BEG
      call WGET (IS ,CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+N
      IN( 3) = IN( 2)+max(M,N)
      IN( 4) = IN( 3)+N*M
      MUX    = IN( 4)+N*N
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('MADANGI')
C
      return
      end
