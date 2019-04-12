      subroutine CASPIA
     $(KODE,K,AHZ,XHZ)
C
C     Rudolf Loeser, 1983 Jul 28
C---- Computes XHZ, an intermediate for
C     emergent Line Spectrum calculations.
C     If KODE=1, then input AHZ is Intensity /Hz;
C     if KODE=2, then input AHZ is Flux /Hz.
C     !DASH
      save
C     !DASH
      real*8 AHZ, PI, XHZ
      integer K, KODE
C     !COM
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (TUNI( 1),PI    )
C     !DASH
      external MOVE1, CONDIV, HI, BYE
C
C               AHZ(K), XHZ(K)
      dimension AHZ(*), XHZ(*)
C
      call HI ('CASPIA')
C     !BEG
      call MOVE1    (AHZ,K,XHZ)
C
      if(KODE.eq.2) then
        call CONDIV (PI,XHZ,K)
      end if
C     !END
      call BYE ('CASPIA')
C
      return
      end
