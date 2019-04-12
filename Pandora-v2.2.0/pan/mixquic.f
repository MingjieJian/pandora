      subroutine MIXQUIC
     $(W,L,NW,EMU,WAVES,XLTIT,KODE,MUX,YY,MYX,BRIGHT,EMINT,YSTAR,
     $ BRIGHTA,EMINTA,YSTARA,LTYPE,XMULT,LFB,TAUK,N,WVNUM,CONINT,
     $ LININT,LINK,XLB3,KLNC,LYNC,WLYNC,XLYNC)
C
C     Rudolf Loeser, 1982 Apr 30
C---- Saves continuous intensity results, for later use.
C     !DASH
      save
C     !DASH
      real*8 BRIGHT, BRIGHTA, EMINT, EMINTA, EMU, TAUK, W, WAVES, WLYNC,
     $       WVNUM, XLB3, XLTIT, XLYNC, XMULT, YSTAR, YSTARA, YY
      integer J, KLNC, KODE, L, LFB, LINK, LTYPE, LYNC, MUX, MYX, N, NW
      logical CONINT, INCRAD, LININT
C     !DASH
C     !EJECT
      external DOWNY, BENT, VOLVOX, CERES, KONTLU, HI, BYE
C
      dimension W(*)
C
C               EMU(L), EMINT(NW,L), KODE(NW,L), MUX(NW,L), EMINTA(NW),
      dimension EMU(*), EMINT(NW,*), KODE(NW,*), MUX(NW,*), EMINTA(*),
C
C               XLTIT(NW), LTYPE(NW), XMULT(NW), TAUK(N,NW), MYX(NW,L),
     $          XLTIT(*),  LTYPE(*),  XMULT(*),  TAUK(*),    MYX(NW,*),
C
C               BRIGHT(NW,L), XLB3(Li3len), WLYNC(KLYNF), XLYNC(KLYNF),
     $          BRIGHT(NW,*), XLB3(*),      WLYNC(*),     XLYNC(*),
C
C               YSTAR(NW,L), YSTARA(NW), WVNUM(NW), BRIGHTA(NW),
     $          YSTAR(NW,*), YSTARA(*),  WVNUM(*),  BRIGHTA(*),
C
C               YY(NW,L), WAVES(NW)
     $          YY(NW,*), WAVES(*)
C
      call HI ('MIXQUIC')
C     !BEG
      if(LINK.eq.3) then
C----   Save data for line profiles calculation
        call VOLVOX (NW, L, WAVES, LTYPE, XLTIT, EMU, BRIGHT, EMINT,
     $               YSTAR, BRIGHTA, EMINTA, YSTARA, XLB3, LFB)
      end if
C
C---- Save data in special Spectrum save file, and
C     for Spectrum Summary
      do 100 J = 1,L
        call DOWNY  (LFB, EMU(J), INCRAD)
        call BENT   (INCRAD, J, EMU(J), NW, WAVES, EMINT(1,J),
     $               YSTAR(1,J), KODE(1,J), MUX(1,J), YY(1,J),
     $               MYX(1,J), BRIGHT(1,J), EMINTA, YSTARA, BRIGHTA,
     $               XLTIT, LTYPE, XMULT, LFB, TAUK, WVNUM, N, CONINT,
     $               LININT)
  100 continue
C
      if(KLNC.gt.0) then
C----   H Ly lines normalization data
        call CERES  (LYNC, WLYNC, XLYNC, NW, WAVES, XLTIT, EMINT)
      end if
C
      if(CONINT) then
C----   Save the mu=1 spectrum (common block chicle)
        call KONTLU (NW, WAVES, EMINT(1,1), YSTAR(1,1))
      end if
C     !END
      call BYE ('MIXQUIC')
C
      return
      end
