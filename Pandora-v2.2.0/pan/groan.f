      subroutine GROAN
     $(X,XLM)
C
C     Rudolf Loeser, 2005 Nov 02
C---- Sets up Background H Ly lines normalization factors, FLNRML,
C     for this XLM.
C     Note: only #2 & #3 are define and needed, thus only they may
C     differ from one. The others are used as formal factors,
C     however, and therefore MUST = 1 (they are initialized by BOX).
C     (This is version 2 of GROAN.)
C     !DASH
      save
C     !DASH
      real*8 ONE, R, X, XLM
      integer IQLNU, IRET, JJFNA, JJFNB, JJWLA, JJWLB, JS, KODE, NFL
C     !COM
C---- LIFFEY      as of 2005 Nov 02
      real*8      FLNRML
      dimension   FLNRML(15)
      common      /LIFFEY/ FLNRML
C     Background H Ly alpha & beta normalization factor for the
C     current value of wavelength (only #2 and #3 can differ from 1)
C     (FLNRML is set up by GROAN)
C     .
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
      equivalence (IZOQ(255),JJWLA)
      equivalence (IZOQ(256),JJFNA)
      equivalence (IZOQ(257),JJWLB)
      equivalence (IZOQ(258),JJFNB)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !EJECT
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
      equivalence (IQQ(340),IQLNU)
C     !DASH
      external PARINT, HI, BYE
C
      dimension X(*)
C
      dimension R(4)
C
      data KODE /1/
C
      call HI ('GROAN')
C     !BEG
      FLNRML(2) = ONE
      FLNRML(3) = ONE
C
      if(IQLNU.gt.0) then
        JS = 0
        call PARINT (X(JJWLA), 1, X(JJFNA), 1, NFL, XLM, FLNRML(2),
     $               KODE, IRET, JS, R)
        if(IRET.ne.1) then
          FLNRML(2) = ONE
        end if
C
        JS = 0
        call PARINT (X(JJWLB), 1, X(JJFNB), 1, NFL, XLM, FLNRML(3),
     $               KODE, IRET, JS, R)
        if(IRET.ne.1) then
          FLNRML(3) = ONE
        end if
      end if
C     !END
      call BYE ('GROAN')
C
      return
      end
