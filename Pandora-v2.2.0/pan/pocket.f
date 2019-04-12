      subroutine POCKET
     $(LU,N,FMC,FMCS,ETA,ETAS,Z,ZL,AMUX,IMUX,IPNT,IVEC,W,ARRL,XNE,XNP,
     $ ZHEL,ZMEL,R,RL)
C
C     Rudolf Loeser, 1982 Jun 07
C---- Plots FMC - relative contributions of electrons;
C           ETA - degrees of ionization; and
C           XNE - electron density (and contributors XNP, ZHEL & ZMEL).
C     (This is version 3 of POCKET.)
C     !DASH
      save
C     !DASH
      real*8 AMUX, ARRL, ETA, ETAS, FMC, FMCS, R, RL, W, XNE, XNP, Z,
     $       ZHEL, ZL, ZMEL, dum
      integer IMUX, IPNT, IVEC, LU, N, NMTS
C     !DASH
      external  NASSAU, OLBIA, BOLL, HI, BYE
C
      dimension W(*)
C
C               FMC(N,NMT), FMCS(N,NMT), ETA(N,NMT), ETAS(N,NMT), Z(N),
      dimension FMC(*),     FMCS(*),     ETA(*),     ETAS(*),     Z(*),
C
C               XNP(N), ZHEL(N), IPNT(NMT), ARRL(N,NMT), XNE(N), ZL(N),
     $          XNP(*), ZHEL(*), IPNT(*),   ARRL(*),     XNE(*), ZL(*),
C
C               ZMEL(N), AMUX(NMT), IMUX(NMT), IVEC(NMT), R(N), RL(N)
     $          ZMEL(*), AMUX(*),   IMUX(*),   IVEC(*),   R(*), RL(*)
C
      call HI ('POCKET')
C     !BEG
C---- Set up sorted subset arrays of FMC and ETA
      call NASSAU  (N,NMTS,FMC,FMCS,ETA,ETAS,AMUX,IMUX,IPNT,IVEC)
C
      if(NMTS.gt.0) then
C----   Plot FMC, with H and He
        call OLBIA (N,NMTS,Z,FMCS,ZL,ARRL,1,XNP,ZHEL,XNE,R,RL,200,LU,
     $              'Relative contributions of electrons')
C----   Plot ETA
        call OLBIA (N,NMTS,Z,ETAS,ZL,ARRL,0,dum,dum,dum,dum,dum,200,LU,
     $              'Degrees of ionization')
      end if
C
C---- Plot XNE
      call BOLL    (N,Z,ZL,XNE,XNP,ZHEL,ZMEL,LU,W)
C     !END
      call BYE ('POCKET')
C
      return
      end
