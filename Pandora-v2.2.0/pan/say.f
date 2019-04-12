      subroutine SAY
     $(LU)
C
C     Rudolf Loeser, 1974 Jun 05
C---- Prints timing and memory data.
C     (This is version 5 of SAY.)
C     !DASH
      save
C     !DASH
      real*8 P, SIXTY, TIME, TIMH, TIMM, TIMS, ZERO
      integer I, J, JE, JS, K, LIPR, LU, LUNU, MASI, MASW, NP, NSEC
      character LINES*38
C     !COM
C---- WORLD       as of 2002 Jun 04
C
      integer     LISTK
      parameter   (LISTK = 100)
      integer     ISTCK,INEXT,ILMIT,IUMAX,IUKNT
      dimension   ISTCK(LISTK)
      common      /WORLD/ ISTCK,INEXT,ILMIT,IUMAX,IUKNT
C     Management of floating point working/scratch storage in X
C     - ISTCK is the allocation stack
C     - INEXT is the stack index for the next allocation
C     - ILMIT is the length of X
C     - IUMAX and IUKNT are cumulative usage statistics.
C     .
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
C
C---- IWORLD      as of 2002 Jun 04
C
      integer     LJSTK
      parameter   (LJSTK = 100)
      integer     JSTCK,JNEXT,JLMIT,JUMAX,JUKNT
      dimension   JSTCK(LJSTK)
      common      /IWORLD/ JSTCK,JNEXT,JLMIT,JUMAX,JUKNT
C     Management of integer working/scratch storage in IX
C     - JSTCK is the allocation stack
C     - JNEXT is the stack index for the next allocation
C     - JLMIT is the length of IX
C     - JUMAX and JUKNT are cumulative usage statistics.
C     .
C---- MINIGER     as of 2006 Jan 12
      integer     JBSCR,JZOQ
      dimension   JZOQ(17)
      common      /MINIGER/ JBSCR,JZOQ
C     INTEGER*4 General Data Block components index.
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
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
C---- MACTAN      as of 1998 Apr 03
      real*8      FILDAT
      dimension   FILDAT(11)
      common      /MACTAN/ FILDAT
C---- Control parameters for the PANDORA random-access scratch file.
C     .
C     !DASH
      external  SECOND, PITH, RAKNTS, LINER, BAKA, HI, BYE
      intrinsic max
C
      dimension NP(4), P(4), NSEC(LINKS), LINES(3)
C
      data NP /8, 15, 4, 5/
      data NSEC /1, 2, 3, 5, 4, 32, 6, 26,
     $           27, 7, 8, 9, 10, 11, 29, 12, 13,
     $           14, 15, 16, 28, 17, 18,
     $           19, 20 ,21, 22,
     $           30, 24, 23, 25, 31/
      data SIXTY /6.D1/
C     !EJECT
C
      call HI ('SAY')
C     !BEG
      write (LU,100)
  100 format(' ','E x e c u t i o n   D a t a',21X,'Total',4X,
     $           'Number',6X,'Storage Needed',19X,'Disk Scratch I/O'/
     $       ' ',46X,'Running',8X,'of',4X,'------------------',5X,
     $           '-------------------------------------'/
     $       ' ',49X,'Time',5X,'Times',4X,'"world"   "iworld"',4X,
     $           2(4X,'# of'),2(6X,'bytes')/
     $       ' ',48X,'(sec)',6X,'done',4X,'(r*8''s)',4X,'(i*4''s)',
     $           7X,'Reads',2X,'Writes',7X,'read',4X,'written')
C
      LIPR = 0
      MASW = 0
      MASI = 0
      JE = 0
      do 107 I = 1,4
        JS = JE+1
        JE = JE+NP(I)
        P(I) = ZERO
        call LINER    (1,LU)
        do 102 J = JS,JE
          K = NSEC(J)
          TIME = TYME(K)
          P(I) = P(I)+TIME
          MASW = max(MASW,MEMSW(K) )
          MASI = max(MASI,MEMSIW(K))
          if(KALLS(K).gt.0) then
            call PITH (XNRR(K),XNRW(K),XNBR(K),XNBW(K),LINES(1))
            write (LU,101) K,SECNAM(K)(:7),SECTIT(K)(:26),TIME,
     $                     KALLS(K),MEMSW(K),MEMSIW(K),LINES(1)
  101       format(' ','[',I3,1X,A7,'] ',A26,F13.2,I10,2I11,4X,A38)
            LIPR = LIPR+1
          end if
  102   continue
        call LINER    (1,LU)
        if(I.eq.1) then
          write (LU,103) P(1)
  103     format(' ',20X,'INITIALIZATION total',F13.2)
        else if(I.eq.2) then
          write (LU,104) P(2)
  104     format(' ',24X,'ITERATIONS total',F13.2)
        else if(I.eq.3) then
          write (LU,105) P(3)
  105     format(' ',19X,'POST-PROCESSING total',F13.2)
        else
          write (LU,106) P(4)
  106     format(' ',26X,'SPECTRUM total',F13.2)
        end if
  107 continue
C     !EJECT
      call BAKA    (LINES)
      TIMS = P(1)+P(2)+P(3)+P(4)
      write (LU,108) TIMS,MASW,MASI,LINES(1),IBSCR,JBSCR,LINES(2),
     $               ILMIT,JLMIT,LINES(3)
  108 format(' ',67X,'------------------',5X,
     $           '-------------------------------------'/
     $       ' ',24X,'PROCESSING total',F13.2,5X,'max =',2I11,4X,A38/
     $       ' ',56X,'(data =',2I11,')',3X,A38/
     $       ' ',56X,'Limit =',2I11,4X,A38)
C
      call SECOND  (TIME)
      TIME = TIME-TYME0
      TIMM = TIME/SIXTY
      TIMH = TIMM/SIXTY
      call LINER   (1,LU)
      write (LU,109) TIME,TIMM,TIMH
  109 format(' ',31X,'RUN total',F13.2,' sec, =',F8.2,' min, =',
     $           F7.2,' hr')
C
      LUNU = LINKS-LIPR
      if(LUNU.gt.0) then
        call LINER (LUNU,LU)
      end if
C     !END
      call BYE ('SAY')
C
      return
      end
