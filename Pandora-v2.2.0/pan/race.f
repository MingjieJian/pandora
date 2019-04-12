      subroutine RACE
     $(X,IX,W,IW,ITAU,N,NL,NSL,MSL,KRJ,PIJ,CIJ,GMI,XND,NOK,BDIJ,RHOIJ,
     $ KIJ,YBAR,KFSV,F,XM,XR,Z,XMS,LEGEND,RACK,FUJ,KILROY)
C
C     Rudolf Loeser, 1981 Feb 13
C---- For the given depth ITAU, RACE computes the basic B-ratios
C     BDIJ for transitions (J,1), where 1 .le. J .le. NL.
C
C---- Upon return, RACK .gt. 0 means - all seems ok;
C                  RACK .le. 0 means - try again with fudged RHOs.
C
C     (This is version 2 of RACE.)
C     !DASH
      save
C     !DASH
      real*8 BDIJ, CIJ, F, GMI, PIJ, RACK, RHOIJ, W, X, XM, XMS, XND,
     $       XR, YBAR, Z
      integer ITAU, IW, IX, JNEG, KFSV, KIJ, KMAT, KODE, KRJ, ML, MSL,
     $        N, NL, NSL
      logical FUJ, KILROY, NOK
      character LEGEND*33
C     !DASH
      external CRASS, MOVE1, TSUNI, YUROK, LINEN, MINGO, CORON, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               PIJ(N,NL**2), CIJ(N,NL**2), GMI(N,NSL), XMS(NL-1,NL-1),
      dimension PIJ(*),       CIJ(*),       GMI(*),     XMS(*),
C
C               BDIJ(N,NL), YBAR(N,NT), KIJ(NL,NL), XR(NL-1), Z(NL,NL),
     $          BDIJ(*),    YBAR(*),    KIJ(*),     XR(*),    Z(*),
C
C               RHOIJ(N,NT), XND(N,NL), XM(NL-1,NL-1)
     $          RHOIJ(*),    XND(*),    XM(*)
C
C     !EJECT
C
      call HI ('RACE')
C     !BEG
C
      ML  = NL-1
C
C---- Get matrix XM and vector XR (and matrix Z)
      call CRASS   (ITAU, N, NL, NSL, MSL, KRJ, PIJ, CIJ, GMI, XND,
     $              NOK, RHOIJ, YBAR, X, IX, Z, XM, XR)
C
C---- Save and invert XM
      call MOVE1   (XM, (ML*ML), XMS)
      call TSUNI   (W, IW, ML, XM, XMS, ITAU, LEGEND, KMAT)
C
C---- Compute basic B-ratios.
      call YUROK   (ML, NL, ITAU, N, XM, KMAT, XR, BDIJ, JNEG, RACK)
C
      if((JNEG.gt.0).and.FUJ) then
C----   Save for iterative summary
        call MINGO (KFSV, ITAU, JNEG, F, RACK)
      end if
C
C---- (? printout; to LUEO)
      call CORON   (ITAU, JNEG, KODE)
      if(KODE.gt.0) then
        call LINEN (KODE, ITAU, JNEG, LEGEND, NL, Z, ML, XMS, XR, XM,
     $              F, RACK, 1, KILROY)
      end if
C     !END
      call BYE ('RACE')
C
      return
      end
