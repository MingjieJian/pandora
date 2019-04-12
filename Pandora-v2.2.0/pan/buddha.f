      subroutine BUDDHA
     $(EMU,WAVES,NW,TAUK,SCON,WS,EMINT,MUX,YY,KODE,BRIGHT,YSTAR,Z,TE,
     $ EMINTA,BRIGHTA,YSTARA,XLTIT,XMULT,CNXP,LTYPE,BANDL,BANDU,WSSAV,
     $ SNUSAV,LEGEND,DIDH,MYX,WVNUM,WTAB,AVCON,W,IW,IJECT,CONINT,
     $ LININT,LINK,XLB3,KLNC,LYNC,WLYNC,XLYNC,NO)
C
C     Rudolf Loeser, 1970 Jan 26
C---- Supervises calculation of emergent intensity.
C     !DASH
      save
C     !DASH
      real*8 AVCON, AVCONA, BANDL, BANDU, BRIGHT, BRIGHTA, CNXP, DIDH,
     $       EMINT, EMINTA, EMU, SCON, SNUSAV, TAUK, TE, W, WAVES,
     $       WLYNC, WS, WSSAV, WTAB, WVNUM, XLB3, XLTIT, XLYNC, XMULT,
     $       YSTAR, YSTARA, YY, Z
      integer IJECT, IQCPU, IQDIC, IW, KLNC, KODE, L, LFB, LFBV, LINK,
     $        LTYPE, LYNC, MUX, MYX, N, NO, NW
      logical CONINT, DODIDH, LEGEND, LININT, SVDIDH
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 7),L  )
      equivalence (JZQ( 1),N  )
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
      equivalence (IQQ(288),IQDIC)
      equivalence (IQQ( 97),IQCPU)
C     !DASH
C     !EJECT
      external VISHNU, SHIVA, MIXQUIC, CARINA, DANGUS, DONATI, KARDA,
     $         ZORA, DURGA, HI, BYE
C
      dimension W(*), IW(*)
C
C               LTYPE(Nmkuse), BRIGHT(Nmkuse,L), YSTAR(Nmkuse,L), Z(N),
      dimension LTYPE(*),      BRIGHT(*),        YSTAR(*),        Z(*),
C
C               BRIGHTA(Nmkuse), WSSAV(N,Nmkuse,L), SNUSAV(N,Nmkuse,L),
     $          BRIGHTA(*),      WSSAV(*),          SNUSAV(*),
C
C               YY(Nmkuse,L), MUX(Nmkuse,L), MYX(Nmkuse,L), BANDL(NAB),
     $          YY(*),        MUX(*),        MYX(*),        BANDL(*),
C
C               BANDU(NAB), YSTARA(Nmkuse), DIDH(N,Nmkuse,L), AVCON(L),
     $          BANDU(*),   YSTARA(*),      DIDH(*),          AVCON(*),
C
C               EMINTA(Nmkuse), KODE(Nmkuse,L), SCON(N,Nmkuse), EMU(L),
     $          EMINTA(*),      KODE(*),        SCON(*),        EMU(*),
C
C               EMINT(Nmkuse,L), CNXP(N,Nmkuse), TAUK(N,Nmkuse), TE(N),
     $          EMINT(*),        CNXP(*),        TAUK(*),        TE(*),
C
C               WAVES(Nmkuse), WVNUM(Nmkuse), XLTIT(Nmkuse), WS(N,L),
     $          WAVES(*),      WVNUM(*),      XLTIT(*),      WS(*),
C
C               XMULT(Nmkuse), WTAB(Nmkuse), XLB3(Li3len),
     $          XMULT(*),      WTAB(*),      XLB3(*),
C
C               WLYNC(KLYNF), XLYNC(KLYNF)
     $          WLYNC(*),     XLYNC(*)
C     !EJECT
C
      call HI ('BUDDHA')
C     !BEG
      DODIDH = (IQDIC.gt.0).and.CONINT
      SVDIDH = (IQCPU.gt.0)
C
C---- Loop over viewing positions
      call ZORA      (LFBV)
      do 100 LFB = 1,LFBV
C
C----   Compute intensity/Hz, and save it (& intermediates) as needed
        call VISHNU  (W, EMU, L, WAVES, WVNUM, WTAB, NW, TAUK, SCON,
     $                Z, WS, EMINT, MUX, YY, KODE, EMINTA, N, CNXP,
     $                WSSAV, SNUSAV, LFB, DIDH, MYX, AVCON, AVCONA,
     $                DODIDH, CONINT, LININT, LINK)
C
C----   Compute brightness temperature and intensity/Angstrom
        call DURGA   (NW, L, WAVES, EMINT, BRIGHT, YSTAR, EMINTA,
     $                BRIGHTA, YSTARA, LFB)
C
C----   Save all results, for possible later use
        call MIXQUIC (W, L, NW, EMU, WAVES, XLTIT, KODE, MUX, YY,
     $                MYX, BRIGHT, EMINT, YSTAR, BRIGHTA, EMINTA,
     $                YSTARA, LTYPE, XMULT, LFB, TAUK, N, WVNUM,
     $                CONINT, LININT, LINK, XLB3, KLNC, LYNC,
     $                WLYNC, XLYNC)
C
C----   Print
        call CARINA  (NO, IW, N, NW, L, WAVES, WTAB, EMU, EMINT,
     $                KODE, MUX, YY, BRIGHT, YSTAR, EMINTA, BRIGHTA,
     $                YSTARA, XMULT, LTYPE, XLTIT, IJECT, LFB, MYX,
     $                AVCON, AVCONA, Z, WSSAV, SNUSAV, LEGEND,
     $                CONINT, LININT, LINK)
C
C----   Print/plot dI/dh
        call DANGUS  (NO, DODIDH, SVDIDH, DIDH, MYX, MUX, YY, XLTIT,
     $                WTAB, LTYPE, NW, Z, TE, N, EMU, L, W, IW)
C----   Plot
        call DONATI  (W, NO, LFB, NW, L, WTAB, LTYPE, EMINT, EMINTA,
     $                BRIGHT, BRIGHTA, LININT)
C
        if(CONINT) then
C----     Compute & print band averages, Composite Opacity wavelengths
          call KARDA (W, IW, BANDL, BANDU, L, EMU, NW, LTYPE, WAVES,
     $                EMINT, YSTAR, BRIGHT, NO, IJECT, LFB)
C
C----     Compute and print color temperature
          call SHIVA (EMU, L, WAVES, NW, EMINT, BRIGHT, NO, IJECT,
     $                LFB, WTAB)
        end if
C
  100 continue
C     !END
      call BYE ('BUDDHA')
C
      return
      end
