      subroutine VIXEN
     $(L,XLM,EMU,XNE,TE,V,H1,VEX,ON1,ONL,OBDU,OBDL,ITAU,DMPI,OPAC)
C
C     Rudolf Loeser, 2004 Apr 16
C---- Computes simulated background O-I line opacity.
C     !DASH
      save
C     !DASH
      real*8 CDL, CRS, DDL, DLK, DW, EMU, GTN, H1, OBDL, OBDU, ON1, ONL,
     $       OPAC, PHI, TE, V, VEX, XLM, XNE
      integer ITAU, L, LDL
      logical DMPI
C     !COM
C---- FURGO       as of 2004 Jun 11
      parameter   (MOXL=11)
      integer     MOXL, IUOX, ILOX
      real*8      OXMAS, OXSKE, OXWVL, OXWLO, OXWHI, OXNUU, OXNUL
      real*8      OXPU,  OXPL,  OXAUL, OXCRD, OXCVW, OXCSK
      dimension   OXWVL(MOXL), OXWLO(MOXL), OXWHI(MOXL), OXNUU(MOXL),
     $            OXNUL(MOXL), OXPU(MOXL),  OXPL(MOXL),  OXAUL(MOXL),
     $            OXCRD(MOXL), OXCVW(MOXL), OXCSK(MOXL),
     $            IUOX(MOXL),  ILOX(MOXL)
      common      /FURGO0/ OXMAS,OXSKE
      common      /FURGO1/ OXWVL,OXWLO,OXWHI
      common      /FURGO2/ OXNUU,OXNUL,OXPU,OXPL
      common      /FURGO3/ OXAUL,OXCRD,OXCVW,OXCSK
      common      /FURGO4/ IUOX,ILOX
C     Data for Oxygen-I lines in the background.
C     .
C     !DASH
      external GITANA, PHILO, HI, BYE
C
      data LDL,DDL,CDL,CRS /1, 0.D0, 1.D0, 0.D0/
C
      call HI ('VIXEN')
C     !BEG
C---- Compute DW and GTN
      call GITANA (TE, V, OXNUU(L), OXNUL(L), OXMAS, OXPU(L), OXPL(L),
     $             OXAUL(L), ONL, OBDU, OBDL, DW, GTN, DMPI, ITAU)
C
      DLK = XLM-OXWVL(L)
C---- Compute DP and PHI
      call PHILO  (EMU, VEX, DLK, OXWVL(L), H1, ON1, XNE, TE, DW, LDL,
     $             DDL, CDL, OXCRD(L), OXCVW(L), OXCSK(L), OXSKE, CRS,
     $             PHI, DMPI, ITAU)
C
      OPAC = GTN*PHI
C     !END
      call BYE ('VIXEN')
C
      return
      end
