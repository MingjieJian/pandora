      subroutine PLUCKY
     $(XLM,XLP,CORE,OXYGEN2,KOPAC,ISWA,N,TE,O2BD,BCKSM,DUMP,ISWE,CEMI)
C
C     Rudolf Loeser, 2007 Jan 22
C---- Supervises calculation of background O-II line source functions.
C     !DASH
      save
C     !DASH
      real*8 BCKSM, CEMI, CORE, O2BD, TE, XLM, XLP
      integer ISWA, ISWE, KOPAC, N
      logical DUMP, OXYGEN2
C     !DASH
      external CRUSTY, HI, BYE
C
C               O2BD(N,Limd), BCKSM(NCSBA), KOPAC(Nopac), CEMI(N,Nlin),
      dimension O2BD(*),      BCKSM(*),     KOPAC(*),     CEMI(*),
C
C               ISWA(Nopac), ISWE(Nopac), TE(N)
     $          ISWA(*),     ISWE(*),     TE(*)
C
      call HI ('PLUCKY')
C     !BEG
      if((KOPAC(44).gt.0).and.(ISWA(44).gt.0)) then
C----   O-II lines
        call CRUSTY (XLM, CORE, OXYGEN2, N, TE, O2BD, DUMP, CEMI)
        ISWE(44) = 1
      end if
C     !END
      call BYE ('PLUCKY')
C
      return
      end
