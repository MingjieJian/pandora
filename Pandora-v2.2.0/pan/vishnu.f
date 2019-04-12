      subroutine VISHNU
     $(W,EMU,L,WAVES,WVNUM,WTAB,NW,TAUK,SCON,Z,WS,EMINT,MUX,YY,KODE,
     $ EMINTA,N,CNXP,WSSAV,SNUSAV,LFB,DIDH,MYX,AVCON,AVCONA,DODIDH,
     $ CONINT,LININT,LINK)
C
C     Rudolf Loeser, 1980 Jun 13
C---- Computes Emergent Continuum Intensity.
C     !DASH
      save
C     !DASH
      real*8 AVCON, AVCONA, CNXP, DIDH, EMINT, EMINTA, EMU, FIVE, SCON,
     $       SNUSAV, TAU2, TAUK, W, WAVES, WS, WSSAV, WTAB, WVNUM, YY,
     $       Z
      integer I, IN, IS, ISNU, ISU, ITMU, KODE, L, LFB, LINK, MOX, MUX,
     $        MYX, N, NW
      logical CONINT, DODIDH, GOOD, LININT
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 6),FIVE  )
C     !DASH
      external RISK, BLADE, SHANK, OTUMBA, BRUCE, FUPPE, SHIVUN, WGIVE,
     $         HI, BYE
C
      dimension W(*)
C
C               EMINT(Nmkuse,L), MUX(Nmkuse,L), YY(Nmkuse,L), AVCON(L),
      dimension EMINT(*),        MUX(*),        YY(*),        AVCON(*),
C
C               WAVES(Nmkuse), KODE(Nmkuse,L), WSSAV(N,Nmkuse,L), Z(N),
     $          WAVES(*),      KODE(*),        WSSAV(*),          Z(*),
C
C               TAUK(N,Nmkuse), SCON(N,Nmkuse), EMINTA(Nmkuse), EMU(L),
     $          TAUK(N,*),      SCON(N,*),      EMINTA(*),      EMU(*),
C
C               MYX(Nmkuse,L), WS(N,L), CNXP(N,Nmkuse), WTAB(Nmkuse),
     $          MYX(*),        WS(*),   CNXP(N,*),      WTAB(*),
C
C               DIDH(N,Nmkuse,L), WVNUM(Nmkuse), SNUSAV(N,Nmkuse,L)
     $          DIDH(*),          WVNUM(*),      SNUSAV(*)
C
      dimension IN(3)
      equivalence
     $(IN( 1),ITMU  ),(IN( 2),ISNU  ),(IN( 3),ISU   )
C     !EJECT
C
      call HI ('VISHNU')
C     !BEG
C     (Get, and allocate, W allotment)
      call SHIVUN    (IN, IS, MOX, 'VISHNU')
C
C---- Loop over all wavelengths
      do 100 I = 1,NW
C
        TAU2 = TAUK(2,I)
        GOOD = TAU2.le.FIVE
        call RISK    (SCON(1,I), N, Z, W(ISNU))
C----   Compute intensity, and related data, for all look-angles
        if(GOOD) then
          call BLADE (I, L, N, NW, TAUK(1,I), EMU, W(ITMU), EMINT,
     $                W(ISNU), WS, MUX, YY, KODE, CNXP(1,I), EMINTA,
     $                WTAB, LFB, Z, DIDH, MYX, W(ISU), LININT, DODIDH,
     $                W)
        else
          call SHANK (I, L, N, NW, TAU2, EMINT, MUX, KODE, YY, EMINTA,
     $                MYX, DIDH, DODIDH)
        end if
C----   Save results, and source function (SU) and intermediate (WS)
        call OTUMBA  (I, L, N, NW, GOOD, LFB, WAVES, EMU, EMINT, WS,
     $                W(ISU), WSSAV, SNUSAV, CONINT)
C
  100 continue
C
C---- Compute averages
      call BRUCE     (EMU, L, AVCON, AVCONA, WVNUM, EMINT, EMINTA, NW,
     $                LFB)
C---- Save checksum
      call FUPPE     (EMU, L, NW, EMINT, LFB, LININT, LINK)
C
C     (Give back W allotment)
      call WGIVE     (W, 'VISHNU')
C     !END
      call BYE ('VISHNU')
C
      return
      end
