      subroutine NIGRA
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 2000 Jul 21
C---- Allocates integer scratch storage for PERROS.
C     !DASH
      save
C     !DASH
      integer IN, IS, KM, LDLMX, MUX
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
      external IGET, ILCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('NIGRA')
C     !BEG
      call IGET (IS,  CALLER)
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+KM
      IN( 3) = IN( 2)+KM
      IN( 4) = IN( 3)+KM
      MUX    = IN( 4)+LDLMX
C
      call ILCK (MUX, CALLER)
C     !END
      call BYE ('NIGRA')
C
      return
      end
