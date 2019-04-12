      subroutine SIAM
     $(X,IX,W,IW,SPHERE,CONINT,CONFLX,ECSPEC,LININT,LINFLX,XLB3,
     $ LEGEND,LINK,KLNC,LYNC,WLYNC,XLYNC,IJECT)
C
C     Rudolf Loeser, 1980 Jun 13
C---- Drives calculation of continuum emission.
C     (This is version 3 of SIAM.)
C     !DASH
      save
C     !DASH
      real*8 W, WLYNC, X, XLB3, XLYNC
      integer ICBL, ICNX, IDX, IFD, IJECT, IKPC, IMG, IN, IPR, IS, ISC,
     $        ITAU, IW, IWAV, IWS, IWTAB, IWVN, IX, IXLT, IXML, JN,
     $        KLNC, LINK, LTY, LU, LYNC, MOX, MUX, NW
      logical BACKGR, CONFLX, CONINT, ECSPEC, LEGEND, LINCON, LINFLX,
     $        LININT, SPHERE
C     !DASH
      external RAMIRO, HARTWIG, BERMUDA, WHEEL, PUELLA, VORPAL, MORTAL,
     $         MOME, MARION, WGIVE, IGIVE, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               XLB3(Li3len), WLYNC(Klynf), XLYNC(Klynf)
      dimension XLB3(*),      WLYNC(*),     XLYNC(*)
C
      dimension IN(11)
      equivalence
     $(IN( 1),IWAV  ),(IN( 2),IXLT  ),(IN( 3),IXML  ),(IN( 4),IWVN  ),
     $(IN( 5),ICBL  ),(IN( 6),ITAU  ),(IN( 7),ISC   ),(IN( 8),IKPC  ),
     $(IN( 9),ICNX  ),(IN(10),IFD   ),(IN(11),IWTAB )
C
      dimension JN(4)
      equivalence
     $(JN( 1),LTY   ),(JN( 2),IPR   ),(JN( 3),IMG   ),(JN( 4),IDX   )
C     !EJECT
C
      call HI ('SIAM')
C     !BEG
C---- Set up context indicators, and verify them
      BACKGR = CONINT.or.CONFLX.or.ECSPEC
      LINCON = LININT.or.LINFLX
      call MORTAL     (BACKGR, LINCON)
C
C---- Set up NMKUSE (in TABLET); needed for MOME and RAMIRO!
      call MARION     (BACKGR, LINCON)
C     (Get, and allocate, W and IW allotments)
      call MOME       (IN, IS,  MOX, 'SIAM')
      call RAMIRO     (JN, IWS, MUX, 'SIAM')
C
C---- Extract data from Continuum Data blocks
      if(BACKGR.or.LINCON) then
        call BERMUDA  (W(ICBL), BACKGR, LINCON, NW, W(IWAV), W(IXLT),
     $                 W(IXML), IW(LTY), W(ITAU), W(ISC), W(IKPC),
     $                 W(ICNX), W(IFD), W(IWVN), W(IWTAB), LINK)
      end if
      if(NW.gt.0) then
C----   Get output unit, and write general header
        call HARTWIG  (LININT, LINFLX, LINK, BACKGR, LU)
        IJECT = 1
C----   Compute intensity
        if(CONINT.or.LININT) then
          call WHEEL  (X, W, IW, NW, W(IWAV), W(IXLT), W(IXML), IW(LTY),
     $                 W(ITAU), W(ISC), W(ICNX), W(IWVN), W(IWTAB),
     $                 LEGEND, IJECT, CONINT, LININT, LINK, XLB3, KLNC,
     $                 LYNC, WLYNC, XLYNC, LU)
        end if
C----   Compute flux
        if(CONFLX.or.LINFLX) then
          call PUELLA (X, W, NW, W(IWAV), W(IXML), IW(LTY), W(IXLT),
     $                 SPHERE, W(ITAU), W(ISC), W(IKPC), W(IFD),
     $                 W(ICNX), W(IWVN), W(IWTAB), IJECT, IW(IMG),
     $                 LINFLX, LINK, XLB3, LU)
        end if
C----   Compute "eclipse" intensity and flux
        if(ECSPEC.and.(.not.LINCON)) then
          call VORPAL (X, W, NW, W(IWAV), W(IWVN), W(IWTAB), IW(LTY),
     $                 IW(IDX), W(ISC), W(IKPC), IW(IPR), IJECT, LU)
        end if
      end if
C
C     (Give back W & IW allotments)
      call WGIVE      (W,  'SIAM')
      call IGIVE      (IW, 'SIAM')
C     !END
      call BYE ('SIAM')
C
      return
      end
