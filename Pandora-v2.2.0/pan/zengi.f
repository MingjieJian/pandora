      subroutine ZENGI
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1995 Aug 11
C---- Allocates integer scratch storage for LILY.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, N, NT
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 5),NT )
C     !DASH
      external IGET, ILCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('ZENGI')
C     !BEG
      call IGET (IS ,CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+NT
      IN( 3) = IN( 2)+NT
      IN( 4) = IN( 3)+NT
      MUX    = IN( 4)+N
C
      call ILCK (MUX,CALLER)
C     !END
      call BYE ('ZENGI')
C
      return
      end
