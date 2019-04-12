      subroutine TOTILA
     $(NO,NVY,N,MRR,K,R1N,Z,FRR,DL,WTAB,SI,DI,SF,DF,TF,W,CORE)
C
C     Rudolf Loeser, 1982 Apr 08
C---- Plots, for LISA.
C     !DASH
      save
C     !DASH
      real*8 CORE, DF, DI, DL, FRR, R1N, SF, SI, TF, W, WTAB, Z
      integer IDPTH, IN, IRADI, IS, ISINT, IXINT, K, MOX, MRR, N, NO,
     $        NRAD, NVY
C     !COM
C---- IMAGE       as of 1997 Aug 21
      integer     IMALEN
      parameter   (IMALEN=65535)
      character   IMAGE*(IMALEN)
      common      /IMAGE/ IMAGE
C     Character string to hold plot images constructed by the
C     K-type line printer plotting routines;
C     but used also as a general scratch character array.
C     .
C     !DASH
      external MARTIN, FLURRY, AMASIA, GELIMER, RHODO, BUZES, WGIVE,
     $         HI, BYE
C
      dimension W(*)
C
C               FRR(MRR), DL(KM), SI(N,KM), DI(MRR,KM), SF(KM), TF(KM),
      dimension FRR(*),   DL(*),  SI(*),    DI(*),      SF(*),  TF(*),
C
C               DF(KM), WTAB(KM), Z(N)
     $          DF(*),  WTAB(*),  Z(*)
C
      dimension IN(4)
      equivalence
     $(IN( 1),ISINT ),(IN( 2),IXINT ),(IN( 3),IRADI ),(IN( 4),IDPTH )
C     !EJECT
C
      call HI ('TOTILA')
C     !BEG
      if(NO.gt.0) then
C       (Get, and allocate, W allotment)
        call MARTIN  (IN,IS,MOX,'TOTILA')
C
C----   Set up data arrays
        call FLURRY  (N,MRR,K,NRAD,R1N,Z,FRR,W(IRADI),W(IDPTH),SI,DI,
     $                W(IXINT))
C----   Plot intensity vs. radius
        call AMASIA  (NO,NVY,IMAGE,W(IXINT),W(IRADI),NRAD,DL,K,W(ISINT))
C----   Plot intensity spectrum
        call GELIMER (NO,NVY,IMAGE,W(IXINT),W(IRADI),NRAD,WTAB,K,
     $                W(ISINT))
C----   Plot flux spectrum
        call RHODO   (NO,NVY,IMAGE,WTAB,K,DF,MRR,SF,TF)
C----   Print scaled intensity array
        call BUZES   (NO,NVY,NRAD,K,W(IRADI),W(IDPTH),WTAB,W(IXINT),
     $                W(ISINT))
C
C       (Give back W allotment)
        call WGIVE   (W,'TOTILA')
      end if
C     !END
      call BYE ('TOTILA')
C
      return
      end
