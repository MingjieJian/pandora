      subroutine SCHEIN
     $(XLM,XLP,CORE,OXYGEN1,KOPAC,ISWA,N,TE,OBD,BCKSM,DUMP,ISWE,CEMI)
C
C     Rudolf Loeser, 2004 Mar 31
C---- Supervises calculation of background O-I line source functions.
C     !DASH
      save
C     !DASH
      real*8 BCKSM, CEMI, CORE, OBD, TE, XLM, XLP
      integer ISWA, ISWE, KOPAC, N
      logical DUMP, OXYGEN1
C     !DASH
      external TUNNY, HI, BYE
C
C               CEMI(N,Nlin), BCKSM(NCSBA), KOPAC(Nopac), ISWE(Nopac),
      dimension CEMI(N,*),    BCKSM(*),     KOPAC(*),     ISWE(*),
C
C               OBD(N,Limd), ISWA(Nopac), TE(N)
     $          OBD(*),      ISWA(*),     TE(*)
C
      call HI ('SCHEIN')
C     !BEG
      if((KOPAC(39).gt.0).and.(ISWA(39).gt.0)) then
C----   O-I lines
        call TUNNY (XLM, CORE, OXYGEN1, N, TE, OBD, DUMP, CEMI)
        ISWE(39) = 1
      end if
C     !END
      call BYE ('SCHEIN')
C
      return
      end
