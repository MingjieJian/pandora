      subroutine ITONOS
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1991 Nov 29
C---- Computes scratch storage requirements for PROFILE.
C     !DASH
      save
C     !DASH
      integer IN, IS, LDLMX, MUX, N
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
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
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('ITONOS')
C     !BEG
      call WGET (IS ,CALLER)
C
      IN( 1) = IS
      MUX    = IN( 1)+N*LDLMX
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('ITONOS')
C
      return
      end
