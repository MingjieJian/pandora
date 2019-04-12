      subroutine MADINGO
     $(IN,IS,MUX,CALLER,N)
C
C     Rudolf Loeser, 1997 Jul 21
C---- Allocates integer scratch storage for MAPPING.
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
      external  IGET, ILCK, HI, BYE
      intrinsic max
C
      dimension IN(*)
C
      call HI ('MADINGO')
C     !BEG
      call IGET (IS ,CALLER)
C
      IN( 1) = IS
      MUX    = IN( 1)+max(N,M)
C
      call ILCK (MUX,CALLER)
C     !END
      call BYE ('MADINGO')
C
      return
      end
