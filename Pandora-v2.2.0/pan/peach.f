      subroutine PEACH
     $(MR,WRANJ,RNUNJ,RCPNJ,YRATE,ICHSW,JDMCI,JDMCE,LINE,NO)
C
C     Rudolf Loeser, 1980 Mar 05
C---- Prints rates integration data.
C     (This is version 2 of PEACH.)
C     !DASH
      save
C     !DASH
      real*8 RCPNJ, RNUNJ, WRANJ, WRTMN, WRTMX, YRATE, YRATS
      integer ICHSW, IEL, IER, IQLYM, IQUTR, IQUWT, J, JDMCE, JDMCI,
     $        KOLEV, KSHEL, LIM, LLEFT, LRITE, MLE, MR, MRI, NO, NSL,
     $        jummy
      character BLANK*1, LINE*120
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(40),NSL)
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
      equivalence (KZQ( 33),KOLEV)
      equivalence (RZQ(166),WRTMN)
      equivalence (RZQ(167),WRTMX)
      equivalence (RZQ(168),YRATS)
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
      equivalence (LEST( 1),KSHEL)
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
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
      equivalence (IQQ( 53),IQUTR)
      equivalence (IQQ( 13),IQLYM)
      equivalence (IQQ( 77),IQUWT)
C     !DASH
      external  PADMA, LINER, ROCKY, MINK, STONY, MURAY, LURAY, CHEAP,
     $          PURAY, HI, BYE
      intrinsic min
C
C               MRZ = MRS+NSL+1
C
C               WRANJ(MRZ), RNUNJ(MRZ), RCPNJ(MRZ), YRATE(MRZ),
      dimension WRANJ(*),   RNUNJ(*),   RCPNJ(*),   YRATE(*),
C
C               MR(NSL+1)
     $          MR(*)
C     !EJECT
C
      call HI ('PEACH')
C     !BEG
      if((IQUTR.le.0).and.(NO.gt.0)) then
C
        call PADMA     (NO, 'Rates Integrations')
        call LURAY     (NO, IQUWT, WRTMN, WRTMX, YRATS)
        call CHEAP     (NO)
        call MURAY     (NO, IQLYM, KOLEV, ICHSW, JDMCI, JDMCE)
        call PURAY     (NO)
C
        LIM = NSL+min(KSHEL,1)
        do 101 J = 1,LIM,2
C
          LLEFT = J
          call ROCKY   (LLEFT, NSL, LINE(1:60))
          call MINK    (LLEFT, MR, jummy, IEL)
          MLE = MR(LLEFT)+1
C
          LRITE = J+1
          if(LRITE.le.LIM) then
            call ROCKY (LRITE, NSL, LINE(61:120))
            call MINK  (LRITE, MR, jummy, IER)
            MRI = MR(LRITE)+1
          else
            LINE(61:) = BLANK
            IER = 1
            MRI = 0
          end if
C
          call LINER   (2, NO)
          write (NO,100) LINE
  100     format(' ',A120)
          call LINER   (1, NO)
          call STONY   (NO,
     $                  LLEFT, LINE( 1: 60), MLE,
     $                  WRANJ(IEL), RNUNJ(IEL), RCPNJ(IEL), YRATE(IEL),
     $                  LRITE, LINE(61:120), MRI,
     $                  WRANJ(IER), RNUNJ(IER), RCPNJ(IER), YRATE(IER))
  101   continue
C
      end if
C     !END
      call BYE ('PEACH')
C
      return
      end
