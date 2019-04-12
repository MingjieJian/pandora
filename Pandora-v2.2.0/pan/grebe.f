      subroutine GREBE
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1981 Nov 05
C---- Allocates scratch storage for KLAMATH.
C     !DASH
      save
C     !DASH
      integer IN, IS, MRR, MUX, N, NRPMX, NSHL
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(15),MRR)
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
      equivalence (LEST( 8),NRPMX)
      equivalence (LEST( 4),NSHL )
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('GREBE')
C     !BEG
      call WGET (IS ,CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+NRPMX*NSHL
      IN( 3) = IN( 2)+N*MRR
      MUX    = IN( 3)+NRPMX
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('GREBE')
C
      return
      end
