      subroutine BASHKIR
     $(N,Z,TE,NW,WAVES,WVNUM,WTAB,TF,SF,DF,TAUK,SCON,FD,OPAC,CNXP,LFB,
     $ XMULT,LTYPE,XLTIT,LU,LINFLX,LINK,XLB3,IMG,W,IJECT)
C
C     Rudolf Loeser, 1984 Jun 22
C---- Computes total continuum flux, using plane-parallel coordinates.
C     (This is version 2 of BASHKIR.)
C     !DASH
      save
C     !DASH
      real*8 CNXP, DF, FD, OPAC, SCON, SF, TAUK, TE, TF, W, WAVES, WTAB,
     $       WVNUM, XLB3, XLTIT, XMULT, YFLUX, Z, dummy
      integer I, I1FX, I2FD, I3CD, I4CO, ICOEF, IFLX, IFXDS, IG, IIDER,
     $        IIDIF, IIFLX, IJCFD, IJCOE, IJECT, IJFLX, IJFXD, IMG, IN,
     $        IQDEF, IROSK, IROST, IS, ISM, ITEFF, LFB, LINK, LTYPE, LU,
     $        LUD, MOX, N, NW
      logical LINFLX, REGULAR, SPHERE
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (RZQ( 30),YFLUX)
      equivalence (KZQ( 78),IFXDS)
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
      equivalence (IQQ(102),IQDEF)
C     !DASH
C     !EJECT
      external KAZAK, ZEUS, IRTYSH, RISK, TWANK, TWINK, TWONK, EPIDOTE,
     $         BRILL, HIPPIAS, EEL, AVAM, ZERO1, APEX, WGIVE, HI, BYE
C
      dimension W(*)
C
C               TAUK(N,Nmkuse), SCON(N,Nmkuse), CNXP(N,Nmkuse), IMG(N),
      dimension TAUK(N,*),      SCON(N,*),      CNXP(N,*),      IMG(*),
C
C               XMULT(Nmkuse), XLTIT(Nmkuse), FD(N,Nmkuse), TF(Nmkuse),
     $          XMULT(*),      XLTIT(*),      FD(N,*),      TF(*),
C
C               WVNUM(Nmkuse), LTYPE(Nmkuse), DF(Nmkuse), WTAB(Nmkuse),
     $          WVNUM(*),      LTYPE(*),      DF(*),      WTAB(*),
C
C               OPAC(N,Nmkuse), SF(Nmkuse), WAVES(Nmkuse), TE(N), Z(N),
     $          OPAC(*),        SF(*),      WAVES(*),      TE(*), Z(*),
C
C               XLB3(Li3len)
     $          XLB3(*)
C
      dimension IN(14)
      equivalence
     $(IN( 1),IJCOE ),(IN( 2),IROSK ),(IN( 3),IROST ),(IN( 4),ITEFF ),
     $(IN( 5),ISM   ),(IN( 6),IFLX  ),(IN( 7),ICOEF ),(IN( 8),IG    ),
     $(IN( 9),IIFLX ),(IN(10),IIDER ),(IN(11),IIDIF ),(IN(12),IJFLX ),
     $(IN(13),IJFXD ),(IN(14),IJCFD )
C
      data SPHERE /.false./
C
      call HI ('BASHKIR')
C     !BEG
C     (Get, and allocate, W allotment)
      call KAZAK  (IN, IS, MOX, 'BASHKIR', NW)
C
      if(LINFLX) then
        LUD = 0
      else
        call ZEUS (LU, IQDEF, LUD)
      end if
      call EEL    (IFXDS, I1FX, I2FD, I3CD, I4CO)
C
      REGULAR = .not.LINFLX
C     !EJECT
C---- Loop over all wavelengths
      do 100 I = 1,NW
C----   Modify source function (if needed)
        call RISK    (SCON(1,I), N, Z, W(ISM))
C----   Compute Flux
        call AVAM    (N, YFLUX, TAUK(1,I), W(ISM), CNXP(1,I), W(IFLX),
     $                TF(I), W(ICOEF), W(IG))
        if(REGULAR) then
C----     Accumulate integrals
          call TWONK (I, NW, N, WAVES, W(IFLX), W(IIFLX), dummy)
          call TWONK (I, NW, N, WAVES, FD(1,I), W(IIDER), dummy)
        end if
C----   Save quantities for detailed printout (if needed)
        call EPIDOTE (LUD, I, NW, N, I1FX, W(IFLX), W(IJFLX), I2FD,
     $                FD(1,I), W(IJFXD), I3CD, W(IIDER), W(IJCFD),
     $                I4CO, W(ICOEF), W(IJCOE))
  100 continue
C
C---- Print selected details (if needed)
      call TWINK     (LUD, WAVES, NW, N, I1FX, W(IJFLX), I2FD, W(IJFXD),
     $                I3CD, W(IJCFD), I4CO, W(IJCOE), YFLUX)
      if(REGULAR) then
C----   Compute Rosseland-mean quantities, and effective temperature
        call BRILL   (Z, TE, OPAC, WAVES, LTYPE, NW, W(IROSK), W(IROST),
     $                W(ITEFF), IMG, W)
C----   Compute integrated differences
        call IRTYSH  (W(IIFLX), W(IIDIF), N)
C----   Print integrated quantities
        call TWANK   (LU, N, Z, W(IIFLX), W(IIDER), W(IIDIF), W(IROSK),
     $                W(IROST), W(ITEFF), LFB)
C----   Save integrated quantities
        call HIPPIAS (N, Z, TE, W(IIFLX), W(IIDER), W(ITEFF))
      end if
C
C---- Initialize unused quantities
      call ZERO1     (SF, NW)
      call ZERO1     (DF, NW)
C---- Print, save results for later use, and plot graphs
      call APEX      (LU, NW, WAVES, WVNUM, WTAB, XMULT, LTYPE, SPHERE,
     $                TF, SF, XLTIT, IJECT, LFB, LINFLX, LINK, XLB3, W)
C
C     (Give back W allotment)
      call WGIVE     (W, 'BASHKIR')
C     !END
      call BYE ('BASHKIR')
C
      return
      end
