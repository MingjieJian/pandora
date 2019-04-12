      subroutine RENDY
     $(LUMR,Z,FLVS,FION,DIONL,DLVSL)
C     Rudolf Loeser, 1998 Jun 26
C---- Puts CENSUS data into restart file.
C     !DASH
      save
C     !DASH
      real*8 DIONL, DLVSL, FION, FLVS, Z
      integer IONST, IQAN1, KAMB, KION, KVLG, LUMR, N
      character QELSM*8
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
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
      equivalence (QZQ(  2),QELSM)
      equivalence (KZQ( 56),IONST)
C
C---- OPTIONS     as of 2007 Jan 12
C
C     Processing and printing control switches.
C
      integer     NOOPT
      parameter   (NOOPT=345)
C     (When NOOPT is changed, FOP, FURRY, REFAULT must be recompiled!)
      integer     IQQ,IQD,IQT
      character   ONAME*8
      dimension   IQQ(NOOPT),IQD(NOOPT),IQT(NOOPT), ONAME(NOOPT)
C
      common      /OPTIONS/ IQQ
C     IQQ is the actual option status.
      common      /OPTION1/ IQD
C     IQD is the default option status.
      common      /OPTION2/ ONAME
C     ONAME is the option name (use 0000 for unused names).
      common      /OPTION3/ IQT
C     IQT is the option type:
C     1 = printout; 2 = calculation; 3 = miscellaneous; 4 = debug.
      equivalence (IQQ(272),IQAN1)
C     !EJECT
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(32),KAMB )
      equivalence (LEST(47),KVLG )
C     !DASH
      external  BUNT, YARDEN, HI, BYE
      intrinsic max
C
C               Z(N), FLVS(N), FION(N), DIONL(N), DLVSL(N)
      dimension Z(*), FLVS(*), FION(*), DIONL(*), DLVSL(*)
C
      call HI ('RENDY')
C     !BEG
      call YARDEN   (LUMR,1,'CENSUS')
      write (LUMR,100) QELSM,IONST,N
C
      call BUNT     (LUMR,Z   ,'Z')
      call BUNT     (LUMR,FLVS,'FLVSL')
      call BUNT     (LUMR,FION,'FIONL')
C
      call YARDEN   (LUMR,2,'CENSUS')
C
  100 format(A8,2X,2I5)
      KION = max(KVLG,KAMB)
C
      if((IQAN1.gt.0).and.((KION.eq.2).or.(KION.eq.3))) then
        call YARDEN (LUMR,1,'CENSOR')
        write (LUMR,100) QELSM,IONST,N
C
        call BUNT   (LUMR,Z    ,'Z')
        call BUNT   (LUMR,DLVSL,'FLVSL')
        call BUNT   (LUMR,DIONL,'FIONL')
C
        call YARDEN (LUMR,2,'CENSOR')
      end if
C     !END
      call BYE ('RENDY')
C
      return
      end
