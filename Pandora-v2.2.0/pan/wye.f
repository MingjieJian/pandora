      subroutine WYE
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1983 Jul 21
C---- Controls that continuum-related processing of an overall iteration
C     which precedes the sub-iterations loop.
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IW, IX, KFCEU, KODE, KTRU, MODE, MOMET
C     !COM
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(17),MOMET)
      equivalence (LEST(69),KFCEU)
C     !DASH
      external META, OUSE, ALDE, BURE, DEE, CINCH, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      data KODE,MODE,KTRU /1, 0, 0/
C
      call HI ('WYE')
C     !BEG
C---- Compute MOMET, the Continuum Calculations update switch
      call META
C
      if(MOMET.eq.1) then
C----   Continuum Calculation
        call OUSE    (KODE, MODE, KTRU, 1, 1, 1, 1, X, IX, W, IW)
C----   Rates
        call ALDE    (X, IX, W, IW)
C----   H minus
        call BURE    (X, IX, W, IW)
C----   Dust Temperature
        call DEE     (X, IX, W, IW)
      else
        if(KFCEU.gt.0) then
C----     Update CIJ
          call CINCH (X, IX, W, IW)
        end if
      end if
C     !END
      call BYE ('WYE')
C
      return
      end
