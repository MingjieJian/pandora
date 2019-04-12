      subroutine CASTOR
     $(N,I,J,K,R1N,WVL,DL,DUMP,ZN,Z,DP,DW,XNE,MPROM,DDL,FDDL,CDL,LDL,
     $ VEX,V,VR,TE,GTN,SUL,COPTRN,BCTRN,DX,FX,GX,TX,SI,INT1,TAU1,STRN,
     $ EXPAND,LABEL,VSW,W,IW)
C
C     Rudolf Loeser, 2000 Jun 20
C---- Computes a value of Shell Intensity, SI, along the ray tangent
C     to depth "I"; and also the associated data TX, TAU1 and INT1.
C     See also: POLLUX.
C
C---- The MOVING case requires data along "extended" rays; for
C     simplicity the STATIC calculation, too, delivers the results
C     in extended form. The following tables are for extended data:
C     DX, FX, GX, and TX.
C
C     (This is version 2 of CASTOR.)
C     !DASH
      save
C     !DASH
      real*8 BCTRN, CDL, COPTRN, DDL, DL, DP, DW, DX, FDDL, FX, GTN, GX,
     $       R1N, SI, STRN, SUL, TAU1, TE, TX, V, VEX, VR, W, WVL, XNE,
     $       Z, ZN, dummy
      integer I, IMAX, INT1, IW, J, K, LDL, MPROM, N, NRP
      logical DUMP, EXPAND, VSW
      character LABEL*(*)
C     !DASH
      external CYNTHIA, FUNADO, GUNDARI, NESTOR, ENRICO, STENTOR, HOPS,
     $         HI, BYE
C
      dimension W(*), IW(*)
C
C               DP(N,LDLMX), DL(KM), DDL(LDLMX), CDL(LDLMX), TX(NRPMX),
      dimension DP(*),       DL(*),  DDL(*),     CDL(*),     TX(*),
C
C               VEX(N), FX(NRPMX), GTN(N), XNE(N), VR(N), COPTRN(N,KM),
     $          VEX(*), FX(*),     GTN(*), XNE(*), VR(*), COPTRN(*),
C
C               BCTRN(N,KM), DX(NROMX), GX(NRPMX), STRN(N,KM), FDDL(N),
     $          BCTRN(*),    DX(*),     GX(*),     STRN(*),    FDDL(*),
C
C               V(N), SUL(NRPMX), TE(N), DW(N), Z(N)
     $          V(*), SUL(*),     TE(*), DW(*), Z(*)
C     !EJECT
C
      call HI ('CASTOR')
C     !BEG
C---- NRP is the number of points along an extended ray
      NRP = 2*I+5
C
C---- Get DX (distances along extended ray)
      call CYNTHIA   (I, Z, DX, ZN, R1N)
C---- Select S, apply frequency-shift if requested, and leave in SUL
      call HOPS      (J, K, DL, STRN, dummy, WVL, VEX, dummy, Z,
     $                'SHELL', I, W, SUL)
C
C---- Compute FX (opacity) and GX (monochromatic source function)
      if(EXPAND) then
        call GUNDARI (N, I, J, WVL, DL, DP, DW, XNE, MPROM, DDL, FDDL,
     $                CDL, LDL, VEX, TE, V, VR, DX, R1N, ZN, Z,
     $                COPTRN, GTN, BCTRN, SUL, FX, GX, DUMP, LABEL,
     $                VSW, W, IW)
      else
        call FUNADO  (N, I, J, WVL, DL, DP, DW, XNE, MPROM, DDL, FDDL,
     $                CDL, LDL, VEX, TE, V, VR, DX, R1N, ZN, Z,
     $                COPTRN, GTN, BCTRN, SUL, FX, GX, DUMP, LABEL,
     $                VSW, W, IW)
      end if
C---- Edit FX
      call STENTOR   (FX, NRP, LABEL)
C
C---- Compute SI (intensity), and TX (optical depth)
      call NESTOR    ((NRP-1), DX, FX, GX, TX, SI, IMAX, DUMP, I, 1,
     $                LABEL)
C---- Get "index" of where TX equals unity
      call ENRICO    (IMAX, I, TX, INT1, TAU1)
C     !END
      call BYE ('CASTOR')
C
      return
      end
