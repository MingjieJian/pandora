      subroutine DEKLA
     $(L,XLM,EMU,XNE,TE,V,H1,VEX,HE1N1,HE1NL,HE1BDU,HE1BDL,ITAU,
     $ DMPI,OPAC)
C
C     Rudolf Loeser, 2005 Jun 27
C---- Computes simulated background He-I line opacity.
C     !DASH
      save
C     !DASH
      real*8 CDL, CRS, DDL, DLK, DW, EMU, GTN, H1, HE1BDL, HE1BDU,
     $       HE1N1, HE1NL, OPAC, PHI, TE, V, VEX, XLM, XNE
      integer ITAU, L, LDL
      logical DMPI
C     !COM
C---- FIRGO       as of 2005 Jul 07
      parameter   (MHEE=4)
      integer     MHEE, IUHEE, ILHEE
      real*8      HEEMAS, HEEWVL, HEEWLO, HEEWHI, HEENUU, HEENUL
      real*8      HEEAUL, HEEPU,  HEEPL,  HEECRD, HEECVW, HEECSK
      real*8      HEESKE
      dimension   HEEWVL(MHEE), HEEWLO(MHEE), HEEWHI(MHEE),
     $            HEENUU(MHEE), HEENUL(MHEE), HEEPU(MHEE),
     $            HEEPL(MHEE),  HEEAUL(MHEE), HEECRD(MHEE),
     $            HEECVW(MHEE), HEECSK(MHEE),
     $            IUHEE(MHEE),  ILHEE(MHEE)
      common      /FIRGO0/ HEEMAS,HEESKE
      common      /FIRGO1/ HEEWVL,HEEWLO,HEEWHI
      common      /FIRGO2/ HEENUU,HEENUL,HEEPU,HEEPL
      common      /FIRGO3/ HEEAUL,HEECRD,HEECVW,HEECSK
      common      /FIRGO4/ IUHEE,ILHEE
C     Data for Helium lines in the background.
C     .
C     !DASH
      external GITANA, PHILO, HI, BYE
C
      dimension CRS(1), CDL(1), DDL(1)
C
      data LDL,DDL,CDL,CRS /1, 0.D0, 1.D0, 0.D0/
C
      call HI ('DEKLA')
C     !BEG
C---- Compute DW and GTN
      call GITANA (TE, V, HEENUU(L), HEENUL(L), HEEMAS, HEEPU(L),
     $             HEEPL(L), HEEAUL(L), HE1NL, HE1BDU, HE1BDL, DW,
     $             GTN, DMPI, ITAU)
C
      DLK = XLM-HEEWVL(L)
C---- Compute DP and PHI
      call PHILO  (EMU, VEX, DLK, HEEWVL(L), H1, HE1N1, XNE, TE, DW,
     $             LDL, DDL, CDL, HEECRD(L), HEECVW(L), HEECSK(L),
     $             HEESKE, CRS, PHI, DMPI, ITAU)
C
      OPAC = GTN*PHI
C     !END
      call BYE ('DEKLA')
C
      return
      end
