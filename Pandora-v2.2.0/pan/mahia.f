      subroutine MAHIA
     $(LIM,NMAX,TAB,N)
C
C     Rudolf Loeser, 2004 Aug 06
C---- Sets up a table of wavelengths between the MHYL'th Hydrogen Lyman
C     line and the Lyman continuum edge, to capture sufficient
C     detail of the Lyman lines opacity there.
C     (This is version 2 of MAHIA.)
C     !DASH
      save
C     !DASH
      real*8 DW, RYDBRG, TAB, WAVE, XK
      integer I, K, LIM, N, NMAX
C     !COM
C---- FERGO       as of 2004 Aug 05
      parameter   (MHYL=24)
      integer     MHYL, IUHY, ILHY
      real*8      HYWVL, HYWLO, HYWHI, HYNUU, HYNUL
      dimension   HYWVL(MHYL), HYWLO(MHYL), HYWHI(MHYL), HYNUU(MHYL),
     $            HYNUL(MHYL), IUHY(MHYL),  ILHY(MHYL)
      common      /FERGO0/ HYWVL,HYWLO,HYWHI
      common      /FERGO1/ HYNUU,HYNUL
      common      /FERGO2/ IUHY,ILHY
C     Data for Hydrogen Lyman lines in the background.
C     .
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (PCON( 4),RYDBRG)
C     !DASH
C     !EJECT
      external HUGINN, HI, BYE
C
C               TAB(NMAX)
      dimension TAB(*)
C
      data K /127/
C
      call HI ('MAHIA')
C     !BEG
      if(LIM.eq.MHYL) then
C
        if((K+N).gt.NMAX) then
C         Error abort
          call HUGINN ((K+N), NMAX, 'MAHIA')
        end if
C
        XK = K+1
        DW = (HYWVL(MHYL)-RYDBRG)/XK
C
        WAVE = RYDBRG
        do 100 I = 1,K
          WAVE   = WAVE+DW
          N      = N+1
          TAB(N) = WAVE
  100   continue
C
      end if
C     !END
      call BYE ('MAHIA')
C
      return
      end
