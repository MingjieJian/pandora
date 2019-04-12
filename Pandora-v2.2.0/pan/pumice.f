      subroutine PUMICE
     $(CORE,DL,K,WAVES)
C
C     Rudolf Loeser, 1982 Dec 10
C---- Sets up a table of wavelengths for
C     Line Source Function background continuum calculations.
C     (This is version 4 of PUMICE.)
C     !DASH
      save
C     !DASH
      real*8 CORE, DL, WAVES
      integer K
C     !DASH
      external MOVE1, CONADD, HI, BYE
C
C               DL(K), WAVES(K)
      dimension DL(*), WAVES(*)
C
C
      call HI ('PUMICE')
C     !BEG
      call MOVE1  (DL,K,WAVES)
      call CONADD (CORE,WAVES,K)
C     !END
      call BYE ('PUMICE')
C
      return
      end
