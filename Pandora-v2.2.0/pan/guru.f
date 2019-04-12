      subroutine GURU
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1997 Jul 15
C---- Allocates integer scratch storage for GRAU.
C     !DASH
      save
C     !DASH
      integer IN, IS, KM, LEN, MRR, MUX, N
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(15),MRR)
      equivalence (JZQ(49),KM )
C     !DASH
      external  IGET, ILCK, HI, BYE
      intrinsic max
C
      dimension IN(*)
C
      call HI ('GURU')
C     !BEG
      call IGET (IS ,CALLER)
C
      LEN = max(N,MRR)*KM
C
      IN( 1) = IS
      MUX    = IN( 1)+LEN
C
      call ILCK (MUX,CALLER)
C     !END
      call BYE ('GURU')
C
      return
      end
