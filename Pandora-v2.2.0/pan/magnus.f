      subroutine MAGNUS
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1997 Jul 14
C---- Allocates integer scratch storage for BERGAMO.
C     (This is version 2 of MAGNUS.)
C     !DASH
      save
C     !DASH
      integer IN, IS, KM, LDLMX, LEN, MUX
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(49),KM )
C
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(33),LDLMX)
C     !DASH
      external  IGET, ILCK, HI, BYE
      intrinsic max
C
      dimension IN(*)
C
      call HI ('MAGNUS')
C     !BEG
      call IGET (IS,  CALLER)
C
      LEN = max(KM,LDLMX)
C
      IN( 1) = IS
C
      MUX    = IN( 1)+LEN
C
      call ILCK (MUX, CALLER)
C     !END
      call BYE ('MAGNUS')
C
      return
      end
