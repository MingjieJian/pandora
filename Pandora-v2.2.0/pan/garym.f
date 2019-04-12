      subroutine GARYM
     $(X,LABEL)
C
C     Rudolf Loeser, 1988 May 04
C---- Prints number densities.
C     (This is version 2 of GARYM.)
C     !DASH
      save
C     !DASH
      real*8 X
      integer JJBDI, JJNK, JJXND, JSTCN, N, NL, NO, NOION
      character LABEL*(*), QIONM*8
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 50),JJNK )
      equivalence (IZOQ( 59),JJXND)
      equivalence (IZOQ( 44),JJBDI)
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 94),NOION)
      equivalence (KZQ( 35),JSTCN)
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
      equivalence (QEST( 1),QIONM)
C     !EJECT
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
C     !DASH
      external LINER, DRAGON, HI, BYE
C
      dimension X(*)
C
      call HI ('GARYM')
C     !BEG
      if((NOION.le.0).and.(JSTCN.le.0)) then
        call LINER  (1, NO)
        write (NO,100) LABEL,QIONM
  100   format(' ',A,' values of ND, NK and BDI for ',A8)
C
        call DRAGON (NO, N, NL, X(JJXND), X(JJNK), X(JJBDI))
      end if
C     !END
      call BYE ('GARYM')
C
      return
      end
