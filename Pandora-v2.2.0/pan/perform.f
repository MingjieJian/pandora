      subroutine PERFORM
     $(LU,LUSD)
C
C     Rudolf Loeser, 1997 Mar 28
C---- Writes performance data to archive file.
C     !DASH
      save
C     !DASH
      integer IOMX, IONST, IPRFA, ISUB, JSTIN, LU, LUSD, N, NL, NONC,
     $        NT
      character QELSM*8, QMODL*8, QNAME*8
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
      equivalence (JZQ( 5),NT )
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
      equivalence (KZQ( 62),IPRFA)
      equivalence (KZQ(  7),JSTIN)
      equivalence (KZQ( 56),IONST)
      equivalence (QZQ(  1),QNAME)
      equivalence (QZQ(  2),QELSM)
      equivalence (QZQ(  3),QMODL)
      equivalence (KZQ(  8),IOMX )
      equivalence (KZQ(  4),ISUB )
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
      equivalence (LEST(29),NONC )
C
C---- CLOCK       as of 1998 Apr 02
      character   BEGDAT*11, ENDDAT*11, BEGTIM*8, ENDTIM*8
      common      /CLOCK/ BEGDAT,ENDDAT,BEGTIM,ENDTIM
C     .
C     !EJECT
C---- VERSION     as of 2006 May 16
      real*8      VERSION
      integer     NVDSCR
      character   VDSCRPT*63
      dimension   VDSCRPT(45)
      common      /VERSION1/ VERSION
      common      /VERSION2/ NVDSCR
      common      /VERSION3/ VDSCRPT
C     Identifier and description of this version of PANDORA.
C     (Values set by subroutine AARDVRK.)
C     .
C---- SHEEPY      as of 1998 Jan 13
      integer     LINKS
      parameter   (LINKS=32)
C     (Be sure to recompile all users when changing LINKS!)
      real*8      TYME,TYME0,XNRW,XNRW0,XNRR,XNRR0,XNBW,XNBW0,
     $            XNBR,XNBR0
      integer     KALLS,MEMSIW,MEMSW
      character   SECNAM*8,SECTIT*32
      dimension   TYME(LINKS),XNRW(LINKS),XNRR(LINKS),XNBW(LINKS),
     $            XNBR(LINKS),KALLS(LINKS),MEMSIW(LINKS),MEMSW(LINKS),
     $            SECNAM(LINKS),SECTIT(LINKS)
      common      /STATS0/ TYME0,  TYME
      common      /STATS1/ XNRW0,  XNRW
      common      /STATS2/ XNRR0,  XNRR
      common      /STATS3/ XNBW0,  XNBW
      common      /STATS4/ XNBR0,  XNBR
      common      /MEMSC/  MEMSW,  MEMSIW
      common      /KALLS/  KALLS
      common      /SECDAT/ SECNAM, SECTIT
C---- Processing-Section data, and performance statistics for this run
C     (kept separately for each section).
C     .
C     SECNAM : module name for section entry;
C     SECTIT : brief section description;
C     TYME   : CPU time, in seconds;
C     XNRW   : count of random-access records written;
C     XNRR   : count of random-access records read;
C     XNBW   : count of random-access data bytes written;
C     XNBR   : count of random-access data bytes read;
C     MEMSW  : maximum amounts of r*8 scratch storage;
C     MEMSIW : maximum amounts of i*4 scratch storage; and
C     KALLS  : number of times invoked.
C     .
C     !DASH
C     !EJECT
      external PERCORD, HI, BYE
C
      call HI ('PERFORM')
C     !BEG
      if((LU.gt.0).and.(IPRFA.gt.0).and.(JSTIN.eq.0)) then
        call PERCORD (LU, VERSION, BEGDAT, BEGTIM, ENDDAT, ENDTIM,
     $                TYME0, QNAME, QELSM, IONST, QMODL, IOMX, ISUB,
     $                N, NL, NT, NONC, LUSD)
      end if
C     !END
      call BYE ('PERFORM')
C
      return
      end
