      subroutine WHEEL
     $(X,W,IW,NW,WAVES,XLTIT,XMULT,LTYPE,TAUK,SCON,CNXP,WVNUM,WTAB,
     $ LEGEND,IJECT,CONINT,LININT,LINK,XLB3,KLNC,LYNC,WLYNC,XLYNC,NO)
C
C     Rudolf Loeser, 1980 Jun 19
C---- Controls BUDDHA, computing emergent continuum intensity.
C     (XLB3 is needed only when LINK = 3.)
C     (This is version 2 of WHEEL.)
C
C
C     (Remember that WHEEL, and BUDDHA-CARINA-LARISA-WEATHER, are used
C     as examples in the description of the storage allocation process
C     in PANDORA.)
C
C
C     !DASH
      save
C     !DASH
      real*8 CNXP, SCON, TAUK, W, WAVES, WLYNC, WTAB, WVNUM, X, XLB3,
     $       XLTIT, XLYNC, XMULT
      integer IAVK, IBRA, IBRI, IDID, IEMA, IEMI, IISA, IJECT, IKOD,
     $        IMAX, IMYX, IN, IS, ISNV, ISTA, IW, IWS, IWSS, IWSV, IY,
     $        JJBNL, JJBNU, JJMU, JJTE, JJZ, JN, KLNC, LINK, LTYPE,
     $        LYNC, MOX, MUX, NO, NW
      logical CONINT, LEGEND, LININT
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ(  4),JJMU )
      equivalence (IZOQ(160),JJBNL)
      equivalence (IZOQ(161),JJBNU)
      equivalence (IZOQ(  7),JJTE )
C     !DASH
C     !EJECT
      external KRISHNA, BUDDHA, RANULF, WGIVE, IGIVE, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               TAUK(N,Nmkuse), CNXP(N,Nmkuse), SCON(N,Nmkuse),
      dimension TAUK(*),        CNXP(*),        SCON(*),
C
C               WAVES(Nmkuse), WVNUM(Nmkuse), XLTIT(Nmkuse),
     $          WAVES(*),      WVNUM(*),      XLTIT(*),
C
C               LTYPE(Nmkuse), XMULT(Nmkuse), WTAB(Nmkuse),
     $          LTYPE(*),      XMULT(*),      WTAB(*),
C
C               XLB3(Li3len), WLYNC(KLYNF), XLYNC(KLYNF)
     $          XLB3(*),      WLYNC(*),     XLYNC(*)
C
      dimension IN(12)
      equivalence
     $(IN( 1),IDID  ),(IN( 2),IWSS  ),(IN( 3),IAVK  ),(IN( 4),IY    ),
     $(IN( 5),IEMI  ),(IN( 6),IBRI  ),(IN( 7),ISTA  ),(IN( 8),IEMA  ),
     $(IN( 9),IBRA  ),(IN(10),IISA  ),(IN(11),IWSV  ),(IN(12),ISNV  )
C
      dimension JN(3)
      equivalence
     $(JN( 1),IKOD  ),(JN( 2),IMAX  ),(JN( 3),IMYX  )
C
      call HI ('WHEEL')
C     !BEG
C     (Get, and allocate, W & IW allotments)
      call KRISHNA (IN, IS , MOX, 'WHEEL')
      call RANULF  (JN, IWS, MUX, 'WHEEL')
C
      call BUDDHA  (X(JJMU), WAVES, NW, TAUK, SCON, W(IWSS), W(IEMI),
     $              IW(IMAX), W(IY), IW(IKOD), W(IBRI), W(ISTA),
     $              X(JJZ), X(JJTE), W(IEMA), W(IBRA), W(IISA), XLTIT,
     $              XMULT, CNXP, LTYPE, X(JJBNL), X(JJBNU), W(IWSV),
     $              W(ISNV), LEGEND, W(IDID), IW(IMYX), WVNUM, WTAB,
     $              W(IAVK), W, IW, IJECT, CONINT, LININT, LINK, XLB3,
     $              KLNC, LYNC, WLYNC, XLYNC, NO)
C
C     (Give back W & IW allotments)
      call WGIVE   (W , 'WHEEL')
      call IGIVE   (IW, 'WHEEL')
C     !END
      call BYE ('WHEEL')
C
      return
      end
