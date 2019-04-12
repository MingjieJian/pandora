      subroutine LOGIN
     $(NPROG)
C
C     Rudolf Loeser, 1979 May 21
C---- Logs in a processing unit.
C     !DASH
      save
C     !DASH
      real*8 TIME, YNBR, YNBW, YNRR, YNRW
      integer IOMX, IOVER, ISUB, ITER, NPROG
      character DAT*11, TIM*8
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
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
      equivalence (LEST( 2),IOVER)
      equivalence (LEST( 3),ITER )
C
C---- MACTAN      as of 1998 Apr 03
      real*8      FILDAT
      dimension   FILDAT(11)
      common      /MACTAN/ FILDAT
C---- Control parameters for the PANDORA random-access scratch file.
C     .
C     !EJECT
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
      external GET_DATE, GET_TIME, SECOND, RAKNTS, HI, BYE
C     !EJECT
C
      call HI ('LOGIN')
C     !BEG
      call GET_DATE (DAT)
      call GET_TIME (TIM)
C
      if((IOVER+ITER).eq.0) then
        write (*,100) DAT,TIM,SECTIT(NPROG)
  100   format(' ',A11,' ',A8,'  #',16X,A32)
      else if(ITER.eq.0) then
        write (*,101) DAT,TIM,IOVER,IOMX,SECTIT(NPROG)
  101   format(' ',A11,' ',A8,'  #',2X,I2,'/'I2,9X,A32)
      else
        write (*,102) DAT,TIM,IOVER,IOMX,ITER,ISUB,SECTIT(NPROG)
  102   format(' ',A11,' ',A8,'  #',2X,I2,'/',I2,2X,I2,'/',I2,2X,A32)
      end if
C
      call RAKNTS  (FILDAT, YNRW, YNBW, YNRR, YNBR)
      XNRW(NPROG) = XNRW(NPROG)-YNRW
      XNBW(NPROG) = XNBW(NPROG)-YNBW
      XNRR(NPROG) = XNRR(NPROG)-YNRR
      XNBR(NPROG) = XNBR(NPROG)-YNBR
C
      call SECOND  (TIME)
      TYME(NPROG)  = TYME(NPROG)-TIME
      KALLS(NPROG) = KALLS(NPROG)+1
C     !END
      call BYE ('LOGIN')
C
      return
      end
