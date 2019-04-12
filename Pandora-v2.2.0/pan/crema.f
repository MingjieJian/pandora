      subroutine CREMA
     $(BRAS,BDJ,BDR,BDQ,RHOP,RHOJ,RHOW,SSTAR,CHI)
C
C     Rudolf Loeser, 1996 Feb 20
C---- Checksums for "RHO+RBD" calculation.
C     (This is version 2 of CREMA.)
C     !DASH
      save
C     !DASH
      real*8 BDJ, BDQ, BDR, BRAS, CHI, RHOJ, RHOP, RHOW, SSTAR
      integer IOVER, ITER, ML, MO, MT, N, NL, NT
      character TIT*40
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
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C     !DASH
      external CHECKER, HI, BYE
C
C               RHOJ(N,NT), RHOW(N,NT), BDR(N,NL), BDQ(N,NL), BDJ(N,NL),
      dimension RHOJ(*),    RHOW(*),    BDR(*),    BDQ(*),    BDJ(*),
C
C               BRAS(N,NT), SSTAR(N,NT), RHOP(N,NT), CHI(N,NT)
     $          BRAS(*),    SSTAR(*),    RHOP(*),    CHI(*)
C     !EJECT
C
      call HI ('CREMA')
C     !BEG
      if(MO.gt.0) then
        write (TIT,100) IOVER,ITER
  100   format('RHO+RBD:',10X,'IOVR=',I3,', ISUB=',I3)
C
        ML = N*NL
        MT = N*NT
C
        TIT(10:13) = 'BDS '
        call CHECKER (BRAS , 1, MT, TIT)
C
        TIT(10:13) = 'BDJ '
        call CHECKER (BDJ  , 1, ML, TIT)
C
        TIT(10:13) = 'BDR '
        call CHECKER (BDR  , 1, ML, TIT)
C
        TIT(10:13) = 'BDQ '
        call CHECKER (BDQ  , 1, ML, TIT)
C
        TIT(10:13) = 'RHOP'
        call CHECKER (RHOP , 1, MT, TIT)
C
        TIT(10:13) = 'RHOJ'
        call CHECKER (RHOJ , 1, MT, TIT)
C
        TIT(10:13) = 'RHOW'
        call CHECKER (RHOW , 1, MT, TIT)
C
        TIT(10:13) = 'S*  '
        call CHECKER (SSTAR, 1, MT, TIT)
C
        TIT(10:13) = 'CHI '
        call CHECKER (CHI  , 1, MT, TIT)
      end if
C     !END
      call BYE ('CREMA')
C
      return
      end
