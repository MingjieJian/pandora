      subroutine SHELLY
     $(XLM,XLP,CORE,HELIUM2,KOPAC,ISWA,N,TE,HE2BD,BCKSM,DUMP,ISWE,CEMI)
C
C     Rudolf Loeser, 2004 Jul 20
C---- Supervises calculation of background He-II line source functions.
C     (This is version 2 of SHELLY.)
C     !DASH
      save
C     !DASH
      real*8 BCKSM, CEMI, CORE, HE2BD, TE, XLM, XLP
      integer ISWA, ISWE, KOPAC, N
      logical DUMP, HELIUM2
C     !DASH
      external SHEKLA, HI, BYE
C
C               HE2BD(N,Limd), BCKSM(NCSBA), KOPAC(Nopac), ISWE(Nopac),
      dimension HE2BD(*),      BCKSM(*),     KOPAC(*),     ISWE(*),
C
C               CEMI(N,Nlin), ISWA(Nopac), TE(N)
     $          CEMI(*),      ISWA(*),     TE(*)
C
      call HI ('SHELLY')
C     !BEG
      if((KOPAC(33).gt.0).and.(ISWA(33).gt.0)) then
C----   He-II lines
        call SHEKLA (XLM, CORE, HELIUM2, N, TE, HE2BD, DUMP, CEMI)
        ISWE(33) = 1
      end if
C     !END
      call BYE ('SHELLY')
C
      return
      end
