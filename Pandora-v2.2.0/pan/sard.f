      subroutine SARD
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1999 Nov 17
C---- Allocates integer scratch storage for NADINE.
C     (This is version 2 of SARD.)
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, NT
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 5),NT )
C     !DASH
      external IGET, ILCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('SARD')
C     !BEG
      call IGET (IS ,CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+NT
      IN( 3) = IN( 2)+NT
      IN( 4) = IN( 3)+NT
      IN( 5) = IN( 4)+NT
      IN( 6) = IN( 5)+NT
      IN( 7) = IN( 6)+NT
      MUX    = IN( 7)+NT
C
      call ILCK (MUX,CALLER)
C     !END
      call BYE ('SARD')
C
      return
      end
