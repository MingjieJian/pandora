      subroutine BRABANT
     $(NDT,XLDT,N,Z,TDUSTN,TDUSTF,OPAC,TAU,S,CNXP,HINT,HT,DBH,COEF,G,
     $ XDT,W,IMG,HK,H,WTD,YFLUX,TLTR,DUMP)
C
C     Rudolf Loeser, 1987 Mar 20
C---- Computes TDUSTF, for QUIVER.
C     (This is version 2 of BRABANT.)
C     !DASH
      save
C     !DASH
      real*8 CNXP, COEF, DBH, FACT, FOUR, G, H, HINT, HK, HT, OPAC, PI,
     $       RAT, S, STFBLZ, TAU, TDUSTF, TDUSTN, TLTR, TN, TP, VALHI,
     $       VALLO, VALUE, W, WEIGHT, WTD, XDT, XLDT, YFLUX, Z, dummy
      integer I, IMG, IQREF, J, N, NDT
      logical DUMP
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 5),FOUR  )
C
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (TUNI( 1),PI    )
      equivalence (PCON(15),STFBLZ)
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
      equivalence (IQQ( 50),IQREF)
C     !DASH
C     !EJECT
      external  AVAM, TWONK, RAGNAR, RADNOR, DIVIDE, ACARI, AVAHI,
     $          HI, BYE
      intrinsic min, max
C
      dimension W(*)
C
C               XLDT(NDT), XDT(NDT), TDUSTF(N), OPAC(N,NDT), TDUSTN(N),
      dimension XLDT(*),   XDT(*),   TDUSTF(*), OPAC(*),     TDUSTN(*),
C
C               TAU(N,NDT), S(N,NDT), H(N,NDT), HINT(N), DBH(N), HK(N),
     $          TAU(N,*),   S(N,*),   H(N,*),   HINT(*), DBH(*), HK(*),
C
C               COEF(N), G(N,N), HT(N), CNXP(N,NDT), Z(N), IMG(N)
     $          COEF(*), G(*),   HT(*), CNXP(N,*),   Z(*), IMG(*)
C
      call HI ('BRABANT')
C     !BEG
C---- Compute monochromatic flux, H, and integrated flux, HINT
      if(DUMP) then
        call ACARI
      end if
      do 100 J = 1,NDT
        call AVAM    (N, YFLUX, TAU(1,J), S(1,J), CNXP(1,J), H(1,J),
     $                dummy, COEF, G)
        call TWONK   (J, NDT, N, XLDT, H(1,J), HINT, WEIGHT)
        if(DUMP) then
          call AVAHI (J, XLDT(J), WEIGHT, N, H(1,J), HINT)
        end if
  100 continue
C
C---- Compute flux-weighted optical depth
      call RAGNAR    (NDT, XLDT, XDT, OPAC, H, N, Z, HK, HT, IMG, W)
C---- Compute DBH
      call RADNOR    (N, HINT, HT, DBH, IMG, W)
C
C---- Compute TDUSTF
      FACT = (PI*WTD)/(FOUR*STFBLZ)
      do 101 I = 1,N
        TN = TDUSTN(I)
        TP = TN**3
        call DIVIDE (DBH(I),TP,RAT)
        VALLO = TN/TLTR
        VALUE = TDUSTN(I)-FACT*RAT
        VALHI = TN*TLTR
        TDUSTF(I) = min(max(VALLO,VALUE),VALHI)
  101 continue
C
      if(IQREF.gt.0) then
        TDUSTF(N) = TDUSTF(N-1)
      end if
C     !END
      call BYE ('BRABANT')
C
      return
      end
