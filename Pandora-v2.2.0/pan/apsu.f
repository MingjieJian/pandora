      subroutine APSU
     $(X,MSFQM)
C
C     Rudolf Loeser, 1981 Mar 31
C---- Controls printing, for BUST.
C     !DASH
      save
C     !DASH
      real*8 X, YPRE
      integer IQSTA, JJAF, JJAS, JJTS, JJXIB, JJXIF, JJXIR, JJXIS, KB,
     $        KF, KR, KS, LU, M, MSFQM, NDW, NO
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 3),M  )
      equivalence (JZQ( 4),KF )
      equivalence (JZQ(38),KB )
      equivalence (JZQ(37),KR )
      equivalence (JZQ(36),KS )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 22),JJTS )
      equivalence (IZOQ(121),JJXIF)
      equivalence (IZOQ(122),JJAF )
      equivalence (IZOQ(150),JJXIB)
      equivalence (IZOQ(148),JJXIR)
      equivalence (IZOQ(146),JJXIS)
      equivalence (IZOQ(147),JJAS )
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
      equivalence (RZQ( 20),YPRE )
      equivalence (KZQ(  1),NDW  )
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
      equivalence (IQQ( 76),IQSTA)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
C     !DASH
      external ZEUS, APPLE, HI, BYE
C
      dimension X(*)
C
      call HI ('APSU')
C     !BEG
      call ZEUS    (NO, IQSTA, LU)
      if(LU.gt.0) then
        call APPLE (X(JJTS), M, YPRE, KF, X(JJXIF), X(JJAF), KB,
     $              X(JJXIB), KR, X(JJXIR), KS, X(JJXIS), X(JJAS),
     $              NDW, MSFQM, LU)
      end if
C     !END
      call BYE ('APSU')
C
      return
      end
