      subroutine HIRAM
     $(X,W,INDEX,KNT,ALB,WAVE)
C
C     Rudolf Loeser, 1993 Sep 10
C---- Drives the preprocessing of Averaged Line Opacity data.
C     !DASH
      save
C     !DASH
      real*8 ALB, ALOMA, ALOMI, W, WAVE, X
      integer IARRL, IELABD, IGD, IN, INDEX, IPT, IQALD, IS, ITAB1,
     $        ITAB2, ITL, ITT, IZT, JJARA, JJHND, JJTE, JJWVA, JJXNE,
     $        JJZ, KAVNP, KAVNT, KAVNZ, KNT, KWA, LU, LWNT, MOX, N, NKA,
     $        NO
      logical ZGRID
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(57),KWA)
      equivalence (JZQ(50),NKA)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ(  9),JJXNE)
      equivalence (IZOQ( 11),JJHND)
      equivalence (IZOQ(227),JJWVA)
      equivalence (IZOQ(228),JJARA)
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
      equivalence (KZQ(143),KAVNZ)
      equivalence (KZQ(141),KAVNT)
      equivalence (KZQ(142),KAVNP)
      equivalence (RZQ(142),ALOMI)
      equivalence (RZQ(143),ALOMA)
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
      equivalence (IQQ(308),IQALD)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
C     !DASH
C     !EJECT
      external SILAS, HERMAN, HUBERT, GUSHER, DULCIMR, CHECKER, WGIVE,
     $         ZEUS, HI, BYE
C
      dimension X(*), W(*)
C
C               ALB(N), INDEX(KNT)
      dimension ALB(*), INDEX(*)
C
      dimension IN(9)
      equivalence
     $(IN( 1),IZT   ),(IN( 2),ITAB1 ),(IN( 3),ITT   ),(IN( 4),IPT   ),
     $(IN( 5),ITAB2 ),(IN( 6),IGD   ),(IN( 7),IARRL ),(IN( 8),ITL   ),
     $(IN( 9),IELABD)
C
      call HI ('HIRAM')
C     !BEG
C     (Get, and allocate, W allotment)
      call SILAS     (IN, IS, MOX, 'HIRAM')
C
      call ZEUS      (NO, IQALD, LU)
      ZGRID = KAVNZ.gt.1
      if(ZGRID) then
        call HERMAN  (LU, N, KWA, NKA, LWNT, KAVNZ, KNT, INDEX, ALB,
     $                WAVE, X(JJZ), ALOMI, ALOMA, W(IZT), W(ITAB1),
     $                W(ITL), W(IARRL), X(JJWVA), X(JJARA), X(JJTE),
     $                X(JJXNE), X(JJHND))
      else
        call GUSHER  (X, N, W(IGD), W(IELABD))
        call HUBERT  (LU, N, KWA, NKA, KAVNT, KAVNP, KNT, INDEX, ALB,
     $                WAVE, X(JJTE), X(JJXNE), W(IGD), ALOMI, ALOMA,
     $                W(ITT), W(IPT), W(ITAB2), W(ITL), W(IARRL),
     $                X(JJWVA), X(JJARA))
      end if
      if(KWA.gt.2) then
        call DULCIMR (LU, ALOMI, ALOMA, INDEX, KNT, X(JJARA),
     $                W(IARRL), X(JJWVA), '"averaged"', KWA)
      end if
      call CHECKER   (X(JJARA), 1, (KWA*N), '"Averaged" Line Opacity')
C
C     (Give back W allotment)
      call WGIVE     (W, 'HIRAM')
C     !END
      call BYE ('HIRAM')
C
      return
      end
