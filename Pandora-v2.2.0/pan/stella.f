      subroutine STELLA
     $(NW,N,MRR,WAVES,XMULT,LTYPE,XLTIT,SPHERE,TF,SF,DF,TE,Z,FRR,IJECT,
     $ TAUK,SCON,OPAC,FD,CNXP,ZU,TEU,TAUKU,SCONU,FDU,OPACU,CNXPU,WVNUM,
     $ WTAB,LINFLX,LINK,XLB3,IMG,W,NO)
C
C     Rudolf Loeser, 1979 Nov 13
C---- Computes and prints Continuum Flux.
C     Completely new, revised version.
C     !DASH
      save
C     !DASH
      real*8 CNXP, CNXPU, DF, FD, FDU, FRR, OPAC, OPACU, SCON, SCONU,
     $       SF, TAUK, TAUKU, TE, TEU, TF, W, WAVES, WTAB, WVNUM, XLB3,
     $       XLTIT, XMULT, Z, ZU
      integer IJECT, IMG, LFB, LFBV, LINK, LTYPE, MRR, N, NO, NW
      logical LINFLX, SPHERE
C     !DASH
C     !EJECT
      external YENISEI, BASHKIR, ZORA, LETO, HI, BYE
C
      dimension W(*)
C
C               WAVES(Nmkuse), XMULT(Nmkuse), WTAB(Nmkuse), TF(Nmkuse),
      dimension WAVES(*),      XMULT(*),      WTAB(*),      TF(*),
C
C               CNXP(N,Nmkuse), DF(Nmkuse), TAUK(N,Nmkuse), SF(Nmkuse),
     $          CNXP(*),        DF(*),      TAUK(*),        SF(*),
C
C               ZU(N), CNXPU(N,Nmkuse), OPACU(N,Nmkuse), LTYPE(Nmkuse),
     $          ZU(*), CNXPU(*),        OPACU(*),        LTYPE(*),
C
C               TAUKU(N,Nmkuse), TEU(N), SCON(N,Nmkuse), WVNUM(Nmkuse),
     $          TAUKU(*),        TEU(*), SCON(*),        WVNUM(*),
C
C               SCONU(N,Nmkuse), XLTIT(Nmkuse), FD(N,Nmkuse), FRR(MRR),
     $          SCONU(*),        XLTIT(*),      FD(*),        FRR(*),
C
C               IMG(N), OPAC(N,Nmkuse), FDU(N,Nmkuse), XLB3(Li3len),
     $          IMG(*), OPAC(*),        FDU(*),        XLB3(*),
C
C               TE(N), Z(N)
     $          TE(*), Z(*)
C
      call HI ('STELLA')
C     !BEG
      if(SPHERE) then
        call YENISEI   (N, Z, MRR, FRR, NW, WAVES, WVNUM, WTAB, TF, SF,
     $                  DF, SCON, OPAC, XMULT, LTYPE, XLTIT, NO, LINFLX,
     $                  LINK, XLB3, W, IJECT)
      else
        call ZORA      (LFBV)
        do 100 LFB = 1,LFBV
          call LETO    (LFB, N, NW, WTAB, Z, TE, TAUK, SCON, FD, OPAC,
     $                  CNXP, ZU, TEU, TAUKU, SCONU, FDU, OPACU, CNXPU)
          call BASHKIR (N, ZU, TEU, NW, WAVES, WVNUM, WTAB, TF, SF, DF,
     $                  TAUKU, SCONU, FDU, OPACU, CNXPU, LFB, XMULT,
     $                  LTYPE, XLTIT, NO, LINFLX, LINK, XLB3, IMG, W,
     $                  IJECT)
  100   continue
      end if
C     !END
      call BYE ('STELLA')
C
      return
      end
