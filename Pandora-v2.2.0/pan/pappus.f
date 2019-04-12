      subroutine PAPPUS
     $(DUMP,CALLER,KVLG,N,Z,VBMB,HND,HEND,XNK,XND,BETA,HE1,HE2K,ZETA,
     $ DZZ,GX1M,GXL,W,IW,LABEL)
C
C     Rudolf Loeser, 1998 Apr 15
C---- Special calculation of GX1, for diffusion.
C     (This is version 2 of PAPPUS.)
C     !DASH
      save
C     !DASH
      real*8 BETA, DZZ, GX1M, GXL, HE1, HE2K, HEND, HND, VBMB, W, XND,
     $       XNK, Z, ZETA
      integer IW, KVLG, N
      logical DUMP
      character CALLER*(*), LABEL*(*)
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external CALLIAS, TREMBLA, TRAMBLE, PACIFIC, ARRMUL, SPUPPA,
     $         HALT, HI, BYE
C
      dimension W(*), IW(*)
C
C               VBMB(N), GX1M(N), DZZ(N), GXL(N,NL), XND(N,NL), HND(N),
      dimension VBMB(*), GX1M(*), DZZ(*), GXL(N,*),  XND(*),    HND(*),
C
C               HEND(N), XNK(N), BETA(N), HE1(N), HE2K(N), ZETA(N),
     $          HEND(*), XNK(*), BETA(*), HE1(*), HE2K(*), ZETA(*),
C
C               Z(N)
     $          Z(*)
C     !EJECT
C
      call HI ('PAPPUS')
C     !BEG
C---- Recompute zeta .....
      if(KVLG.eq.1) then
        call CALLIAS (N, HND, HEND, VBMB, XNK, XND, ZETA)
      else if(KVLG.eq.2) then
        call TREMBLA (N, HND, HEND, VBMB, BETA, HE2K, XND, ZETA)
      else if(KVLG.eq.3) then
        call TRAMBLE (N, HND, HEND, VBMB, BETA, HE1, XNK, ZETA)
      else
        write (MSSLIN(1),100) KVLG
  100   format('KVLG =',I12,', which is not 1, 2, or 3.')
        call HALT ('PAPPUS', 1)
      end if
C
C---- ..... and its derivative, = DZZ, .....
      call PACIFIC   (Z, ZETA, DZZ, N, W, IW)
C
C---- ..... and multiply that by GX1M to get GX-1
      call ARRMUL    (GX1M, DZZ, GXL, N)
C
      if(DUMP) then
        call SPUPPA  (CALLER, LABEL, N, Z, VBMB, ZETA, DZZ, GX1M,
     $                GXL(1,1))
      end if
C     !END
      call BYE ('PAPPUS')
C
      return
      end
