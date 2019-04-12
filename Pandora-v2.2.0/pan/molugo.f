      subroutine MOLUGO
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1998 Apr 15
C---- Allocates integer scratch storage for COLUGO.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, N
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C     !DASH
      external IGET, ILCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('MOLUGO')
C     !BEG
      call IGET (IS ,CALLER)
C
      IN( 1) = IS
      MUX    = IN( 1)+N
C
      call ILCK (MUX,CALLER)
C     !END
      call BYE ('MOLUGO')
C
      return
      end
