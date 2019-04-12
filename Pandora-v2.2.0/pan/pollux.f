      subroutine POLLUX
     $(N,I,J,K,R1N,WVL,DL,DUMP,FRR,Z,DP,DW,XNE,MPROM,DDL,FDDL,CDL,LDL,
     $ VEX,V,VR,TE,GTN,SUL,COPTRN,BCTRN,DX,FX,GX,TX,DI,INT1,TAU1,STRN,
     $ EXPAND,LABEL,VSW,W,IW)
C
C     Rudolf Loeser, 2000 Jun 20
C---- Computes a value of Disk Intensity, DI, and
C     associated data TX, TAU1 and INT1.
C     See also: CASTOR.
C
C     (This is version 2 of POLLUX.)
C     !DASH
      save
C     !DASH
      real*8 BCTRN, CDL, COPTRN, DDL, DI, DL, DP, DW, DX, FDDL, FRR, FX,
     $       GTN, GX, R1N, STRN, SUL, TAU1, TE, TX, V, VEX, VR, W, WVL,
     $       XNE, Z, dummy
      integer I, IMAX, INT1, IW, J, K, LDL, MPROM, N
      logical DUMP, EXPAND, VSW
      character LABEL*(*)
C     !DASH
      external CLARUS, ARVON, KILYDD, NESTOR, PORREX, STENTOR, HOPS,
     $         HI, BYE
C
      dimension W(*), IW(*)
C
C               DP(N,LDLMX), DW(N), DDL(LDLMX), CDL(LDLMX), STRN(N,KM),
      dimension DP(*),       DW(*), DDL(*),     CDL(*),     STRN(*),
C
C               XNE(N), VR(N), FX(NRPMX), GX(NRPMX), DX(NRPMX), VEX(N),
     $          XNE(*), VR(*), FX(*),     GX(*),     DX(*),     VEX(*),
C
C               COPTRN(N,KM), TX(NRPMX), GTN(N), DL(KM), FDDL(N), V(N),
     $          COPTRN(*),    TX(*),     GTN(*), DL(*),  FDDL(*), V(*),
C
C               TE(N), BCTRN(N,KM), SUL(NRPMX), Z(N)
     $          TE(*), BCTRN(*),    SUL(*),     Z(*)
C     !EJECT
C
      call HI ('POLLUX')
C     !BEG
C---- NRP is the number of points along the ray
C     NRP = N
C
C---- Get DX (distances along ray)
      call CLARUS   (N, Z, FRR, DX, W)
C---- Select S, apply frequency shift if requested, and leave in SUL
      call HOPS     (J, K, DL, STRN, dummy, WVL, VEX, FRR, Z, 'DISK',
     $               I, W, SUL)
C
C---- Compute FX (opacity) and GX (monochromatic source function)
      if(EXPAND) then
        call ARVON  (N, J, DL, DP, DW, XNE, MPROM, DDL, FDDL, CDL,
     $               LDL, VEX, TE, V, VR, DX, R1N, FRR, Z, WVL,
     $               COPTRN, GTN, BCTRN, SUL, FX, GX, DUMP, LABEL,
     $               VSW, W, IW)
      else
        call KILYDD (N, J, DL, DP, DW, XNE, MPROM, DDL, FDDL, CDL,
     $               LDL, VEX, TE, V, VR, DX, R1N, FRR, Z, WVL,
     $               COPTRN, GTN, BCTRN, SUL, FX, GX, DUMP, LABEL,
     $               VSW, W, IW)
      end if
C---- Edit FX
      call STENTOR  (FX, N, LABEL)
C
C---- Compute DI (intensity) and TX (optical depth along ray)
      call NESTOR   ((N-1), DX, FX, GX, TX, DI, IMAX, DUMP, I, 2,
     $               LABEL)
C---- Get "index" of where TX equals unity
      call PORREX   (IMAX, TX, INT1, TAU1)
C     !END
      call BYE ('POLLUX')
C
      return
      end
