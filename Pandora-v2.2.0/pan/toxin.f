      subroutine TOXIN
     $(L,TE,OBDU,OBDL,ITAU,DMPI,S)
C
C     Rudolf Loeser, 2004 Apr 19
C---- Computes simulated O-I background line source function.
C     !DASH
      save
C     !DASH
      real*8 OBDL, OBDU, S, TE
      integer ITAU, L
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
      external ESSEN, HI, BYE
C
      call HI ('TOXIN')
C     !BEG
      call ESSEN (OXNUU(L), OXNUL(L), TE, OBDU, OBDL, S, DMPI, ITAU)
C     !END
      call BYE ('TOXIN')
C
      return
      end
