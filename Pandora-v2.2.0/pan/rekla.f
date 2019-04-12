      subroutine REKLA
     $(L,TE,HE1BDU,HE1BDL,ITAU,DMPI,S)
C
C     Rudolf Loeser, 2005 Jun 24
C---- Computes simulated He-I background line source function.
C     !DASH
      save
C     !DASH
      real*8 HE1BDL, HE1BDU, S, TE
      integer ITAU, L
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
      external ESSEN, HI, BYE
C
      call HI ('REKLA')
C     !BEG
      call ESSEN (HEENUU(L), HEENUL(L), TE, HE1BDU, HE1BDL, S, DMPI,
     $            ITAU)
C     !END
      call BYE ('REKLA')
C
      return
      end
