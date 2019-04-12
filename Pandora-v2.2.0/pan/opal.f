      subroutine OPAL
     $(X)
C
C     Rudolf Loeser, 2005 Nov 04
C---- Writes new values of FNRLMA,B into .msc
C     (This is version 2 of OPAL.)
C     !DASH
      save
C     !DASH
      real*8 X
      integer JJFNA, JJFNB, KLYNF, LUMR, NFL
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(16),NFL)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(256),JJFNA)
      equivalence (IZOQ(258),JJFNB)
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
      equivalence (LEST(81),KLYNF)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS(29),LUMR )
C     !DASH
C     !EJECT
      external YARDEN, PUNT, HI, BYE
C
      dimension X(*)
C
      call HI ('OPAL')
C     !BEG
      if(KLYNF.gt.0) then
        call YARDEN (LUMR, 1, 'FNORML')
        call PUNT   (LUMR, X(JJFNA), NFL, 1, 'FNRMLA')
        call PUNT   (LUMR, X(JJFNB), NFL, 1, 'FNRMLB')
        call YARDEN (LUMR, 2, 'FNORML')
      end if
C     !END
      call BYE ('OPAL')
C
      return
      end
