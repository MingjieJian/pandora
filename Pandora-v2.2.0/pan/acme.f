      subroutine ACME
     $(NO,NW,WAVE,FLUX,IMAGE,LINFLX)
C
C     Rudolf Loeser, 1982 Apr 28
C---- Makes plots, for KLUNG.
C     (This is version 2 of ACME.)
C     !DASH
      save
C     !DASH
      real*8 FLUX, WAVE
      integer NO, NW
      logical LINFLX, OK
      character IMAGE*(*)
C     !DASH
      external LUCCA, LUISA, KPRINT, HI, BYE
C
C               WAVE(NW), FLUX(NW)
      dimension WAVE(*),  FLUX(*)
C
      call HI ('ACME')
C     !BEG
C---- Set up and initialize plot image
      call LUCCA    (IMAGE, WAVE, FLUX, NW, LINFLX, OK)
      if(OK) then
C----   Enter results into plot image
        call LUISA  (NW, WAVE, FLUX, IMAGE)
C----   Print plot
        call KPRINT (IMAGE, NO)
      end if
C     !END
      call BYE ('ACME')
C
      return
      end
