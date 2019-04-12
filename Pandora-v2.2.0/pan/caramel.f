      subroutine CARAMEL
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1980 Jun 12
C---- Prints all sets of restart Jnu values,
C     and updates restart file with latest Z values, if necessary.
C     (This is version 2 of CARAMEL.)
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IDL, IJL, IJNU, IJU, IKK, IN, INN, IQPPJ, IS, IW, IWS, IX,
     $        IZ, JJZ, JN, KMAX, KOUNT, LU, LUJO, MOX, MUX, N, NEWZ,
     $        NMAX, NO, NONC
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 37),JJZ  )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
      equivalence (LUNITS(18),LUJO )
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
      equivalence (LEST(29),NONC )
      equivalence (LEST(34),NEWZ )
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
      equivalence (IQQ(167),IQPPJ)
C     !DASH
C     !EJECT
      external LEAF, FELLOE, ZEUS, SPOKE, PIGWEED, AKABA, IGIVE, WGIVE,
     $         TYRE, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      dimension IN(3)
      equivalence
     $(IN( 1),IDL   ),(IN( 2),IZ    ),(IN( 3),IJNU  )
C
      dimension JN(4)
      equivalence
     $(JN( 1),IJU   ),(JN( 2),IJL   ),(JN( 3),IKK   ),(JN( 4),INN   )
C
      call HI ('CARAMEL')
C     !BEG
C---- Get table lengths
      call PIGWEED (LUJO, NMAX, KMAX)
C
C     (Get, and allocate, W & IW allotments)
      call LEAF    (IN, IS,  MOX, 'CARAMEL', NMAX, KMAX)
      call AKABA   (JN, IWS, MUX, 'CARAMEL')
C
C---- Read
      call FELLOE  (LUJO, NMAX, KMAX, NONC, W(IZ), W(IDL), W(IJNU),
     $              IW(IJU), IW(IJL), IW(IKK), IW(INN), KOUNT)
C---- Print
      call ZEUS    (NO, IQPPJ, LU)
      call SPOKE   (LU, NMAX, KMAX, KOUNT, IW(IJU), IW(IJL), IW(IKK),
     $              IW(INN), W(IJNU), W(IDL))
C---- Update
      if(NEWZ.gt.0) then
        call TYRE  (N, NMAX, KMAX, X(JJZ), W(IDL), W(IJNU), IW(IJU),
     $              IW(IJL), IW(IKK), IW(INN), KOUNT)
      end if
C
C     (Give back W & IW allotments)
      call WGIVE   (W,  'CARAMEL')
      call IGIVE   (IW, 'CARAMEL')
C     !END
      call BYE ('CARAMEL')
C
      return
      end
