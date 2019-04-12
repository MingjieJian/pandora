      subroutine HYTHE
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1983 Sep 02
C---- Allocates scratch storage for KNUT.
C     !DASH
      save
C     !DASH
      integer IN, IS, KM, MUX, NRPMX
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
      equivalence (LEST( 8),NRPMX)
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('HYTHE')
C     !BEG
      call WGET (IS ,CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+NRPMX
      IN( 3) = IN( 2)+NRPMX
      IN( 4) = IN( 3)+NRPMX
      IN( 5) = IN( 4)+NRPMX
      IN( 6) = IN( 5)+NRPMX
      IN( 7) = IN( 6)+KM
      IN( 8) = IN( 7)+NRPMX*KM
      MUX    = IN( 8)+NRPMX
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('HYTHE')
C
      return
      end
