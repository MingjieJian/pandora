      subroutine HUMBLE
     $(X,W,IW,GD,COOL,COOLINT)
C
C     Rudolf Loeser, 1979 Oct 26
C---- Controls final Cooling Rates calculation.
C     !DASH
      save
C     !DASH
      real*8 COOL, COOLINT, GD, W, X
      integer ICLSM, ICOL, ICONA, ICONX, ICRL, ICRT, IHFF, IHMFF, ILINS,
     $        IN, INP, IQCCI, IQSCD, IS, ISUMH, ITPF, IVEC, IW, IXCBL,
     $        IXLB1, IXPBL, IXRAY, IXVAL, JJAIJ, JJBDI, JJBDL, JJCRH,
     $        JJFRS, JJHND, JJMSS, JJPTO, JJPTU, JJRKQ, JJRLQ, JJTE,
     $        JJVT, JJXND, JJXNE, JJXNU, JJZ, KCOL, KCOND, KCRH, KHFF,
     $        KLNS, KRAY, LU, LUCR, MODE, MOX, N, NL, NO, NOION, NSL,
     $        NT, jummy
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
      equivalence (JZQ( 5),NT )
      equivalence (JZQ(40),NSL)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 59),JJXND)
      equivalence (IZOQ( 44),JJBDI)
      equivalence (IZOQ(125),JJBDL)
      equivalence (IZOQ( 67),JJRLQ)
      equivalence (IZOQ( 66),JJRKQ)
      equivalence (IZOQ( 26),JJXNU)
      equivalence (IZOQ( 32),JJAIJ)
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ(  9),JJXNE)
      equivalence (IZOQ( 58),JJPTO)
      equivalence (IZOQ( 57),JJPTU)
      equivalence (IZOQ( 11),JJHND)
      equivalence (IZOQ(119),JJMSS)
      equivalence (IZOQ( 63),JJCRH)
      equivalence (IZOQ( 49),JJVT )
      equivalence (IZOQ(138),JJFRS)
C     !EJECT
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
      equivalence (LUNITS(10),LUCR )
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
      equivalence (KZQ( 94),NOION)
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
      equivalence (LEST(48),KCRH )
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
      equivalence (IQQ(140),IQCCI)
      equivalence (IQQ(134),IQSCD)
C     !DASH
C     !EJECT
      external LACY, GANELON, BOG, BAG, VILA, BAGGY, BOGGY, ZEUS, ROHU,
     $         RUE, POPIO, SWAMP, PILE, POMO, ARGO, MAGMA, WGIVE,
     $         HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               COOL(N), COOLINT(N), GD(N)
      dimension COOL(*), COOLINT(*), GD(*)
C
      dimension IN(18)
      equivalence
     $(IN( 1),IXLB1 ),(IN( 2),ICONX ),(IN( 3),ICRL  ),(IN( 4),ICRT  ),
     $(IN( 5),IXPBL ),(IN( 6),ITPF  ),(IN( 7),INP   ),(IN( 8),ICOL  ),
     $(IN( 9),IHMFF ),(IN(10),IHFF  ),(IN(11),IXCBL ),(IN(12),IVEC  ),
     $(IN(13),ICONA ),(IN(14),ISUMH ),(IN(15),IXRAY ),(IN(16),ILINS ),
     $(IN(17),ICLSM ),(IN(18),IXVAL )
C
      call HI ('HUMBLE')
C     !BEG
C     (Get, and allocate, W allotment)
      call LACY    (IN,IS,MOX,'HUMBLE')
C     (Initialize populations buffer)
      call POPIO   ('INIT',jummy,W(IXPBL))
C
      if(NOION.le.0) then
C----   Cooling rates for bound-free transitions
        call BAG   (N,NSL,NL,X(JJXND),X(JJBDI),X(JJBDL),X(JJRLQ),
     $              X(JJRKQ),W(ICRL))
C----   Cooling rates for bound-bound transitions
        call BOG   (W(IXLB1),N,NL,NT,X(JJXNU),X(JJXND),X(JJAIJ),
     $              W(ICRT))
      end if
C---- Cooling rates for H- free-free and H free-free
      call ARGO    (W(IHMFF),W(IHFF),W(IXCBL),X(JJTE),W,KHFF)
C---- Cooling rates for Composite Lines
      call RUE     (W(IXCBL),W,W(ILINS),KLNS)
C---- Cooling rates for X-rays
      call ROHU    (W(IXCBL),W,W(IXRAY),KRAY)
C---- Cooling rates for CO-lines
      call POMO    (W(IXCBL),W,W(ICOL),KCOL)
C---- Conductive flux gradient
      call VILA    (X(JJZ),X(JJTE),X(JJXNE),W(IXPBL),W(ICONA),
     $              W(ICONX),KCOND,W(IVEC),W)
C---- Total rate
      call GANELON (N,NL,NT,W(ICRL),W(ICRT),KHFF,W(IHFF),W(IHMFF),
     $              W(ISUMH),KCRH,X(JJCRH),KCOND,W(ICONX),KLNS,
     $              W(ILINS),KRAY,W(IXRAY),KCOL,W(ICOL),COOL)
C---- Smoothed rate
      call MAGMA   (COOL,N,W(ICLSM),W(IXVAL),W,IW)
C---- Auxiliary data
      call BOGGY   (N,W(IXPBL),W(INP),X(JJPTO),X(JJPTU),W(ITPF))
C     !EJECT
      MODE = 1
C---- Print
      call BAGGY   (NO,N,NL,NT,X(JJMSS),X(JJZ),X(JJTE),W(ICRL),W(ICRT),
     $              X(JJCRH),KCRH,X(JJHND),X(JJXNE),W(INP),X(JJVT),
     $              X(JJPTO),W(ITPF),GD,W(IHMFF),W(IHFF),KHFF,W(ICONA),
     $              W(ICONX),KCOND,MODE,W(ISUMH),COOL,W(ILINS),KLNS,
     $              W(IXRAY),KRAY,W(ICOL),KCOL,W(ICLSM))
C---- Save for other uses
      call ZEUS    (LUCR,IQSCD,LU)
      call SWAMP   (LU,MODE,N,NL,NT,X(JJZ),X(JJMSS),X(JJTE),W(ICRT),
     $              W(ICRL),X(JJCRH),KCRH,X(JJHND),X(JJXNE),W(INP),
     $              X(JJVT),X(JJPTO),W(ITPF),GD,W(IHMFF),W(IHFF),KHFF,
     $              W(ICONX),KCOND,COOL,W(ILINS),KLNS,W(IXRAY),KRAY,
     $              W(ICOL),KCOL,W(ICLSM))
      if(IQCCI.gt.0) then
C----   Integrated rates
        MODE = 2
C
        call PILE    (N,NT,NL,X(JJZ),W(ICRT),W(ICRL),X(JJCRH),KCRH,
     $                W(IHMFF),W(IHFF),KHFF,W(ICONA),W(ICONX),KCOND,
     $                W(IVEC),X(JJFRS),W(ILINS),KLNS,W(IXRAY),KRAY,
     $                W(ICOL),KCOL)
        call GANELON (N,NL,NT,W(ICRL),W(ICRT),KHFF,W(IHFF),W(IHMFF),
     $                W(ISUMH),KCRH,X(JJCRH),KCOND,W(ICONX),KLNS,
     $                W(ILINS),KRAY,W(IXRAY),KCOL,W(ICOL),COOLINT)
        call BAGGY   (NO,N,NL,NT,X(JJMSS),X(JJZ),X(JJTE),W(ICRL),
     $                W(ICRT),X(JJCRH),KCRH,X(JJHND),X(JJXNE),W(INP),
     $                X(JJVT),X(JJPTO),W(ITPF),GD,W(IHMFF),W(IHFF),
     $                KHFF,W(ICONA),W(ICONX),KCOND,MODE,W(ISUMH),
     $                COOLINT,W(ILINS),KLNS,W(IXRAY),KRAY,W(ICOL),KCOL,
     $                W(ICLSM))
        call SWAMP   (LU,MODE,N,NL,NT,X(JJZ),X(JJMSS),X(JJTE),W(ICRT),
     $                W(ICRL),X(JJCRH),KCRH,X(JJHND),X(JJXNE),W(INP),
     $                X(JJVT),X(JJPTO),W(ITPF),GD,W(IHMFF),W(IHFF),
     $                KHFF,W(ICONX),KCOND,COOLINT,W(ILINS),KLNS,
     $                W(IXRAY),KRAY,W(ICOL),KCOL,W(ICLSM))
      end if
C
C     (Give back W allotment)
      call WGIVE     (W,'HUMBLE')
C     !END
      call BYE ('HUMBLE')
C
      return
      end
