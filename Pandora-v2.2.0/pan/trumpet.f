      subroutine TRUMPET
     $(XLM,XLP,CORE,HELIUM1,KOPAC,ISWA,N,TE,HE1BD,BCKSM,DUMP,ISWE,CEMI)
C
C     Rudolf Loeser, 2005 Jun 24
C---- Supervises calculation of background He-I line source functions.
C     !DASH
      save
C     !DASH
      real*8 BCKSM, CEMI, CORE, HE1BD, TE, XLM, XLP
      integer ISWA, ISWE, KOPAC, N
      logical DUMP, HELIUM1
C     !DASH
      external KELLY, HI, BYE
C
C               HE1BD(N,Limd), BCKSM(NCSBA), KOPAC(Nopac), ISWE(Nopac),
      dimension HE1BD(*),      BCKSM(*),     KOPAC(*),     ISWE(*),
C
C               CEMI(N,Nlin), ISWA(Nopac), TE(N)
     $          CEMI(*),      ISWA(*),     TE(*)
C
      call HI ('TRUMPET')
C     !BEG
      if((KOPAC(35).gt.0).and.(ISWA(35).gt.0)) then
C----   He-II lines
        call KELLY (XLM, CORE, HELIUM1, N, TE, HE1BD, DUMP, CEMI)
        ISWE(35) = 1
      end if
C     !END
      call BYE ('TRUMPET')
C
      return
      end
