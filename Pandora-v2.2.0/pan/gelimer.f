      subroutine GELIMER
     $(NO,NVY,IMAGE,XINT,RADI,NRAD,WTAB,K,SINT)
C
C     Rudolf Loeser, 1982 Apr 12
C---- Plots Intensity vs. Delta-Lambda, for TOTILA.
C     !DASH
      save
C     !DASH
      real*8 PADI, RADI, SINT, WTAB, XINT, XLL, XRL, YLL, YUL
      integer JE, JS, K, KODE, KOUNT, NO, NP, NRAD, NVY
      character IMAGE*(*), qummy*1
C     !DASH
      external BERND, ETHAN, SESTUS, MELIC, ONION, COMITO, HI, BYE
C
C               SINT(NP,KM), XINT(NRAD,KM), RADI(NRAD), WTAB(KM)
      dimension SINT(*),     XINT(*),       RADI(*),    WTAB(*)
C
      dimension PADI(9)
C
      data NP /9/
      data PADI /0.D0, .5D0, 1.D0, 2.D0, 5.D0, 10.D0, 20.D0, 50.D0,
     $           100.D0/
C
      call HI ('GELIMER')
C     !BEG
C---- Get sample selectors
      call BERND      (RADI,NRAD,PADI,NP,JS,JE)
      KOUNT = JE-JS+1
      if(KOUNT.gt.0) then
C----   Extract data to be plotted
        call ETHAN    (XINT,NRAD,K,RADI,PADI(JS),KOUNT,SINT)
C----   Get graph limits
        call SESTUS   (SINT,WTAB,K,KOUNT,YUL,YLL,XLL,XRL,KODE)
        if(KODE.eq.1) then
C----     Initialize graph image
          call MELIC  (IMAGE,XLL,XRL,YLL,YUL)
C----     Enter points
          call ONION  (K,KOUNT,WTAB,SINT,IMAGE,qummy,1)
C----     Print graph
          call COMITO (NO,IMAGE,NVY,PADI(JS),KOUNT)
        end if
      end if
C     !END
      call BYE ('GELIMER')
C
      return
      end
