      subroutine MEEK
C
C     Rudolf Loeser, 1977 Jan 18
C---- Initializes execution performance statistics.
C     (This is version 6 of MEEK.)
C     !DASH
      save
C     !COM
C---- MACTAN      as of 1998 Apr 03
      real*8      FILDAT
      dimension   FILDAT(11)
      common      /MACTAN/ FILDAT
C---- Control parameters for the PANDORA random-access scratch file.
C     .
C---- CLOCK       as of 1998 Apr 02
      character   BEGDAT*11, ENDDAT*11, BEGTIM*8, ENDTIM*8
      common      /CLOCK/ BEGDAT,ENDDAT,BEGTIM,ENDTIM
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
      external GET_DATE, GET_TIME, RAKNTS, SECOND, HI, BYE
C     !EJECT
C
      call HI ('MEEK')
C     !BEG
      call GET_TIME (BEGTIM)
      call GET_DATE (BEGDAT)
      call SECOND   (TYME0)
      call RAKNTS   (FILDAT,XNRW0,XNBW0,XNRR0,XNBR0)
C     !END
      call BYE ('MEEK')
C
      return
      end
