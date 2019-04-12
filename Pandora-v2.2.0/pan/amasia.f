      subroutine AMASIA
     $(NO,NVY,IMAGE,XINT,RADI,NRAD,DL,K,SINT)
C
C     Rudolf Loeser, 1982 Apr 12
C---- Plots Intensity vs. Radius, for TOTILA.
C     !DASH
      save
C     !DASH
      real*8 DL, PLAM, PWAV, RADI, SINT, XINT, XLL, XRL, YLL, YUL
      integer IS, J, JE, JS, K, KODE, KOUNT, NO, NP, NRAD, NVY
      character IMAGE*(*)
C     !DASH
      external  BERND, COLCHIS, PRISCUS, ANDREW, CHIVE, BASSUS, HI, BYE
C
C               SINT(NRAD,NP), XINT(NRAD,KM), RADI(NRAD), DL(K)
      dimension SINT(NRAD,*),  XINT(*),       RADI(*),    DL(*)
C
      dimension PLAM(13), PWAV(13)
C
      data NP /13/
      data PLAM /-30.D0, -10.D0, -3.D0, -1.D0, -.3D0, -.1D0, 0.D0,
     $           .1D0, .3D0, 1.D0, 3.D0, 10.D0, 30.D0/
C
      call HI ('AMASIA')
C     !BEG
C---- Get sample selectors
      call BERND       (DL,K,PLAM,NP,JS,JE)
      KOUNT = JE-JS+1
      if(KOUNT.gt.0) then
C----   Extract data to be plotted
        call COLCHIS   (XINT,NRAD,K,DL,PLAM(JS),KOUNT,SINT)
C----   Get graph limits
        call PRISCUS   (SINT,RADI,NRAD,KOUNT,YUL,YLL,XLL,XRL,IS,KODE)
        if(KODE.eq.1) then
C----     Initialize graph image
          call ANDREW  (IMAGE,XLL,XRL,YLL,YUL)
C----     Enter points
          do 100 J = 1,KOUNT
            call CHIVE (IMAGE,RADI,NRAD,IS,SINT(1,J),J)
  100     continue
C----     Print graph
          call BASSUS  (NO,IMAGE,NVY,PLAM(JS),KOUNT,PWAV)
        end if
      end if
C     !END
      call BYE ('AMASIA')
C
      return
      end
