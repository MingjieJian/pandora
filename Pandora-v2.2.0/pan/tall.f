      subroutine TALL
     $(XLM,XLP,CORE,OXYGEN3,KOPAC,ISWA,N,TE,O3BD,BCKSM,DUMP,ISWE,CEMI)
C
C     Rudolf Loeser, 2007 Jan 22
C---- Supervises calculation of background O-III line source functions.
C     !DASH
      save
C     !DASH
      real*8 BCKSM, CEMI, CORE, O3BD, TE, XLM, XLP
      integer ISWA, ISWE, KOPAC, N
      logical DUMP, OXYGEN3
C     !DASH
      external FALL, HI, BYE
C
C               O3BD(N,Limd), BCKSM(NCSBA), KOPAC(Nopac), CEMI(N,Nlin),
      dimension O3BD(*),      BCKSM(*),     KOPAC(*),     CEMI(*),
C
C               ISWA(Nopac), ISWE(Nopac), TE(N)
     $          ISWA(*),     ISWE(*),     TE(*)
C
      call HI ('TALL')
C     !BEG
      if((KOPAC(45).gt.0).and.(ISWA(45).gt.0)) then
C----   O-II lines
        call FALL (XLM, CORE, OXYGEN3, N, TE, O3BD, DUMP, CEMI)
        ISWE(45) = 1
      end if
C     !END
      call BYE ('TALL')
C
      return
      end
