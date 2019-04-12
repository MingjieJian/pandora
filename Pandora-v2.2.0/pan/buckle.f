      subroutine BUCKLE
     $(W,IW,NEWLYM,XLM,N,NOPAC,KOPAC,ISWA,TE,XNE,XNC,V,HN,BDH,
     $ XLMXX,XLMDR,CO,COMP,XLYB)
C
C     Rudolf Loeser, 2003 Feb 11
C---- Supervises calculation of H Lyman lines background absorbers.
C     !DASH
      save
C     !DASH
      real*8 BDH, CO, COMP, HN, TE, V, W, XLM, XLMDR, XLMXX, XLYB, XNC,
     $       XNE
      integer ILYAA, ILYAN, ILYDN, ILYEN, ILYEU, ILYP, ILYPL, ILYPN,
     $        ILYSA, ILYSF, ILYST, ILYTN, ILYUL, ILYVC, ILYVL, ILYWN,
     $        ILYXL, ILYXX, ISWA, IW, KOPAC, N, NOPAC
      logical DOIT, NEWLYM, SWITCH
C     !COM
C---- LYSTER      as of 2006 Nov 01
      integer     NLL,NLLY,ILB,LENLYB
      logical     INIHLL,INDPTH
      parameter   (NLLY=500)
      dimension   ILB(18)
      common      /LYSTER1/ NLL,LENLYB,ILB
      common      /LYSTER2/ INIHLL,INDPTH
C     Hydrogen Lyman lines background absorber paraphernalia
C     .
      equivalence
     $(ILB( 1),ILYWN),(ILB( 2),ILYEN),(ILB( 3),ILYDN),(ILB( 4),ILYAN),
     $(ILB( 5),ILYPN),(ILB( 6),ILYAA),(ILB( 7),ILYXX),(ILB( 8),ILYSA),
     $(ILB( 9),ILYSF),(ILB(10),ILYST),(ILB(11),ILYTN),(ILB(12),ILYP ),
     $(ILB(13),ILYPL),(ILB(14),ILYUL),(ILB(15),ILYVL),(ILB(16),ILYXL),
     $(ILB(17),ILYVC),(ILB(18),ILYEU)
C     .
C     !DASH
      external WYMANA, ZERO1, HELLA, SCATLY, RELLA, ROWAN, RONGA, WARTS,
     $         HI, BYE
C
      dimension W(*), IW(*)
C
C               CO(Nopac,N), ISWA(Nopac), TE(N), HN(N,Limdat(1)), V(N),
      dimension CO(*),       ISWA(*),     TE(*), HN(*),           V(*),
C
C               XLMXX(LLY), XLMDR(LLY), BDH(N,Limdat(1)), XLYB(Lenlyb),
     $          XLMXX(*),   XLMDR(*),   BDH(*),           XLYB(*),
C
C               KOPAC(Nopac), COMP(N), XNE(N), XNC(N)
     $          KOPAC(*),     COMP(*), XNE(*), XNC(*)
C
      call HI ('BUCKLE')
C     !BEG
      SWITCH = (KOPAC(11)+KOPAC(16)+KOPAC(34)+KOPAC(36)+KOPAC(37)).gt.0
      if(NEWLYM.and.SWITCH) then
C----   Initialize
        call RONGA (N, TE, XNC, XLYB(ILYEN), XLYB(ILYWN), XLYB(ILYDN),
     $              XLYB(ILYAN), XLYB(ILYSA), XLYB(ILYSF), XLM)
        call ZERO1 (COMP, N)
      end if
C     !EJECT
C---- Lyman alpha absorption
      call WARTS    (11, NEWLYM, KOPAC, CO, NOPAC, N, DOIT)
      if(DOIT) then
        call WYMANA (11, XLM, N, NOPAC, TE, XNE, V, HN, XLMXX, XLMDR,
     $               XLYB(ILYEN), XLYB(ILYWN), XLYB(ILYAN),
     $               XLYB(ILYSA), XLYB(ILYSF), XLYB(ILYVC), CO, COMP)
        ISWA(11) = 1
      end if
C
C---- Lyman alpha scattering
      call WARTS    (16, NEWLYM, KOPAC, CO, NOPAC, N, DOIT)
      if(DOIT) then
        call SCATLY (16, XLM, N, NOPAC, TE, XNE, V, HN, XLMXX, XLMDR,
     $               XLYB(ILYEN), XLYB(ILYWN), XLYB(ILYAN),
     $               XLYB(ILYSA), XLYB(ILYSF), XLYB(ILYVC), CO, COMP)
        ISWA(16) = 1
      end if
C
C---- Higher (N = 3, ... 15) Lyman lines absorption
      call WARTS    (34, NEWLYM, KOPAC, CO, NOPAC, N, DOIT)
      if(DOIT) then
        call HELLA  (34, XLM, N, NOPAC, TE, XNE, V, HN, XLMXX, XLMDR,
     $               XLYB(ILYEN), XLYB(ILYWN), XLYB(ILYAN),
     $               XLYB(ILYSA), XLYB(ILYSF), XLYB(ILYVC), CO, COMP,
     $               XLYB(ILYP))
        ISWA(34) = 1
      end if
C
C---- Higher (N = 3, ... 15) Lyman lines scattering
      call WARTS    (36, NEWLYM, KOPAC, CO, NOPAC, N, DOIT)
      if(DOIT) then
        call RELLA  (36, XLM, N, NOPAC, TE, XNE, V, HN, XLMXX, XLMDR,
     $               XLYB(ILYEN), XLYB(ILYWN), XLYB(ILYAN),
     $               XLYB(ILYSA), XLYB(ILYSF), XLYB(ILYVC), CO, COMP,
     $               XLYB(ILYP))
        ISWA(36) = 1
      end if
C
C---- Highest (N > 15; limit = NLL) Lyman lines absorption
      call WARTS    (37, NEWLYM, KOPAC, CO, NOPAC, N, DOIT)
      if(DOIT) then
        call ROWAN  (37, XLM, N, NOPAC, TE, XNE, XNC, V, HN, BDH, CO,
     $               XLYB(ILYPL), XLYB(ILYUL), XLYB(ILYEU), XLYB(ILYVL),
     $               XLYB(ILYXL), XLYB(ILYEN), XLYB(ILYWN), XLYB(ILYDN),
     $               XLYB(ILYAN), XLYB(ILYSA), XLYB(ILYSF), XLYB(ILYTN),
     $               XLYB(ILYPN), XLYB(ILYST), XLYB(ILYAA), XLYB(ILYXX))
        ISWA(37) = 1
      end if
C     !END
      call BYE ('BUCKLE')
C
      return
      end
