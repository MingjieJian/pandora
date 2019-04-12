      subroutine DEVI
     $(X,W,INDEX,KNT,ALB,WAVE)
C
C     Rudolf Loeser, 1973 Jul 24
C---- Drives the preprocessing of Statistical Opacity data.
C     !DASH
      save
C     !DASH
      real*8 ALB, CURMA, CURMI, W, WAVE, X
      integer IARRL, IELABD, IGD, IN, INDEX, IQKUP, IS, ISTEP, IWT,
     $        JJARK, JJFKR, JJHND, JJTE, JJWVK, JJXNE, KNT, KNW, KUDNT,
     $        KURIN, KURU, LU, LWNT, MOX, N, NKA, NO
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(50),NKA)
      equivalence (JZQ(25),KNW)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ(  9),JJXNE)
      equivalence (IZOQ( 11),JJHND)
      equivalence (IZOQ(127),JJWVK)
      equivalence (IZOQ(128),JJARK)
      equivalence (IZOQ( 64),JJFKR)
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
      equivalence (RZQ( 48),CURMI)
      equivalence (RZQ( 49),CURMA)
      equivalence (KZQ( 27),KURIN)
      equivalence (KZQ( 50),KUDNT)
      equivalence (KZQ(152),LWNT )
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
      equivalence (IQQ( 20),IQKUP)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
      equivalence (LUNITS(17),KURU )
C     !DASH
C     !EJECT
      external MAYA, ZEUS, ARIL, WGIVE, HI, BYE
C
      dimension X(*), W(*)
C
C               ALB(N), INDEX(KNT)
      dimension ALB(*), INDEX(*)
C
      dimension IN(5)
      equivalence
     $(IN( 1),IWT   ),(IN( 2),ISTEP ),(IN( 3),IGD   ),(IN( 4),IARRL ),
     $(IN( 5),IELABD)
C
      call HI ('DEVI')
C     !BEG
C     (Get, and allocate, W allotment)
      call MAYA  (IN, IS, MOX, 'DEVI')
C
      call ZEUS  (NO, IQKUP, LU)
      call ARIL  (X, KURIN, LU, KURU, N, KNW, NKA, LWNT, W(IWT),
     $            W(ISTEP), X(JJTE), X(JJXNE), W(IGD), CURMI, CURMA,
     $            INDEX, KNT, ALB, X(JJWVK), X(JJARK), KUDNT,
     $            X(JJFKR), W(IARRL), WAVE, X(JJHND), W(IELABD))
C
C     (Give back W allotment)
      call WGIVE (W, 'DEVI')
C     !END
      call BYE ('DEVI')
C
      return
      end
