      subroutine WERRA
     $(IN,IS,MUX,CALLER,K)
C
C     Rudolf Loeser, 1997 Jul 15
C---- Allocates integer scratch storage for FULDA.
C     !DASH
      save
C     !DASH
      integer IN, IS, K, MUX, NK
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
      call HI ('WERRA')
C     !BEG
      call IGET (IS ,CALLER)
C
      NK = N*K
C
      IN( 1) = IS
      MUX    = IN( 1)+NK
C
      call ILCK (MUX,CALLER)
C     !END
      call BYE ('WERRA')
C
      return
      end
