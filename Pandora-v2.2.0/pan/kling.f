      subroutine KLING
     $(SPHERE,ADS,R1N,NW,WT,TF,FR,FHZ,FA,XLA,TB,FMM,XMG,WREF,VEC)
C
C     Rudolf Loeser, 1981 Sep 10
C---- Computes derived quantities, for Continuum Flux printout.
C
C     Given:
C     WT  - wavelength, and
C     TF  - total Flux;
C
C     Compute:
C     FR  - frequency,
C     FHZ - Flux/Hz,
C     FA  - Flux/Angstrom,
C     XLA - luminosity,
C     TB  - brightness temperature,
C     FMM - Flux at Earth/micrometer, and
C     XMG - Flux ratio in magnitudes.
C     !DASH
      save
C     !DASH
      real*8 A2, ADS, C28, C38, C39, CMPKM, FA, FHZ, FMM, FOUR, FR,
     $       FRQUNT, FW, H1, PI, R1N, T, TB, TF, TFHZP, TFHZS, TLA, VEC,
     $       WL2, WREF, WT, XLA, XMG
      integer I, NW
      logical SPHERE
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
      equivalence (TUNI( 3),FRQUNT)
      equivalence (TUNI( 5),CMPKM )
C     !DASH
      external RIGEL, ANGIE, SEA, MAMA, HI, BYE
C
C               FMM(Nmkuse), XMG(Nmkuse), VEC(Nmkuse), FHZ(Nmkuse),
      dimension FMM(*),      XMG(*),      VEC(*),      FHZ(*),
C
C               XLA(Nmkuse), FA(Nmkuse), TB(Nmkuse), WT(Nmkuse),
     $          XLA(*),      FA(*),      TB(*),      WT(*),
C
C               TF(Nmkuse), FR(Nmkuse)
     $          TF(*),      FR(*)
C     !EJECT
C
      call HI ('KLING')
C     !BEG
      call RIGEL   (38,C38)
      call RIGEL   (39,C39)
      call RIGEL   (28,C28)
      TFHZP = FOUR*PI
      TFHZS = (R1N*CMPKM)**2
      A2  = ADS**2
      TLA = TFHZS*TFHZP
C
      do 100 I = 1,NW
C
        WL2 = WT(I)**2
        if(SPHERE) then
          FHZ(I) = TF(I)/TFHZS
          FMM(I) = ((A2*TF(I))/WL2)*C39
          H1 = FHZ(I)/TFHZP
        else
          H1 = TF(I)
          FHZ(I) = H1*TFHZP
          FMM(I) = ((A2*H1)/WL2)*C38
        end if
        T = (C38*H1)/WL2
        call SEA   (WT(I),T,TB(I))
C
        call ANGIE (WT(I),FW)
        FR(I)  = FRQUNT*FW
        FA(I)  = (C28*FHZ(I))/WL2
        XLA(I) = TLA*FA(I)
  100 continue
C
      call MAMA    (NW,WT,FHZ,XMG,WREF,VEC)
C     !END
      call BYE ('KLING')
C
      return
      end
