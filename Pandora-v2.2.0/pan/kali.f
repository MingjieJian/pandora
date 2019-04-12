      subroutine KALI
     $(X,IX,W,IW,INDEX,KNT,ALB,WAVE)
C
C     Rudolf Loeser, 1983 Jun 29
C---- Drives the preprocessing of Composite Line Opacity Data.
C     !DASH
      save
C     !DASH
      real*8 ALB, FABD, W, WAVE, X
      integer IC01, IC02, IC03, IC04, IC05, IC06, ICLOP, ICOMP, IDEN,
     $        IIPJ, IITJ, IIVJ, IN, INDEX, IPGS, IQKOP, IS, ITABP,
     $        ITABT, ITABV, IW, IWS, IX, JJARC, JJBNL, JJBNU, JJH2N,
     $        JJHND, JJIKW, JJTE, JJV, JJWVC, JJXNE, JN, KNT, KODNT,
     $        KOMNP, KOMNT, KOMNV, KOMPO, KPSP, KWC, LU, LWNT, MOX,
     $        MULT, MUX, N, NAB, NCP, NKA, NO
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(50),NKA)
      equivalence (JZQ(45),NAB)
      equivalence (JZQ(44),NCP)
      equivalence (JZQ(46),KWC)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 11),JJHND)
      equivalence (IZOQ(140),JJH2N)
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ(  9),JJXNE)
      equivalence (IZOQ(157),JJWVC)
      equivalence (IZOQ(158),JJARC)
      equivalence (IZOQ(160),JJBNL)
      equivalence (IZOQ(161),JJBNU)
      equivalence (IZOQ( 12),JJV  )
C
C---- MINIGER     as of 2006 Jan 12
      integer     JBSCR,JZOQ
      dimension   JZOQ(17)
      common      /MINIGER/ JBSCR,JZOQ
C     INTEGER*4 General Data Block components index.
      equivalence (JZOQ(  6),JJIKW)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
      equivalence (LUNITS(26),KOMPO)
C     !EJECT
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 69),KOMNP)
      equivalence (KZQ( 70),KOMNT)
      equivalence (KZQ( 68),KOMNV)
      equivalence (KZQ( 71),KODNT)
      equivalence (RZQ( 61),FABD )
      equivalence (KZQ(152),LWNT )
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
      equivalence (IQQ(170),IQKOP)
C     !DASH
C     !EJECT
      external EDRA, ZEUS, AMBRO, RAINIER, WGIVE, IGIVE, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               INDEX(KNT), ALB(N)
      dimension INDEX(*),   ALB(*)
C
      dimension IN(13)
      equivalence
     $(IN( 1),IDEN  ),(IN( 2),IPGS  ),(IN( 3),ICOMP ),(IN( 4),IC01  ),
     $(IN( 5),IC02  ),(IN( 6),IC03  ),(IN( 7),IC04  ),(IN( 8),ITABV ),
     $(IN( 9),IC06  ),(IN(10),IC05  ),(IN(11),ITABP ),(IN(12),ITABT ),
     $(IN(13),ICLOP )
C
      dimension JN(5)
      equivalence
     $(JN( 1),IITJ  ),(JN( 2),IIPJ  ),(JN( 3),IIVJ  ),(JN( 4),KPSP  ),
     $(JN( 5),MULT  )
C
      call HI ('KALI')
C     !BEG
C     (Get, and allocate, W & IW allotments)
      call EDRA    (IN, IS,  MOX, 'KALI')
      call RAINIER (JN, IWS, MUX, 'KALI')
C
      call ZEUS   (NO, IQKOP, LU)
      call AMBRO  (X, W, N, NAB, NCP, LWNT, KWC, KOMNP, KOMNT, KOMNV,
     $             LU, KOMPO, KODNT, INDEX, FABD, KNT, NKA, X(JJHND),
     $             X(JJH2N), X(JJTE), X(JJXNE), X(JJV), X(JJWVC),
     $             IX(JJIKW), X(JJARC), X(JJBNL), X(JJBNU), ALB,
     $             W(IDEN), W(IPGS), W(IC01), W(IC02), W(IC03),
     $             W(IC04), W(IC05), W(IC06), IW(IITJ), IW(IIPJ),
     $             IW(IIVJ), IW(KPSP), W(ITABP), W(ITABT), W(ITABV),
     $             W(ICLOP), W(ICOMP), IW(MULT), WAVE)
C
C     (Give back W & IW allotments)
      call WGIVE  (W,  'KALI')
      call IGIVE  (IW, 'KALI')
C     !END
      call BYE ('KALI')
C
      return
      end
