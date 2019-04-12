      subroutine ROSATE
     $(IDIR, XLM,XLMC,NC, XLMB,NB, IWGRBS)
C
C     Rudolf Loeser, 1996 Feb 28
C---- Picks a "batch" of wavelengths:
C     down from XLM if IDIR = -1, and up from XLM if IDIR = +1.
C     !DASH
      save
C     !DASH
      real*8 XLM, XLMB, XLMC
      integer I, IDIR, IFLG, IMAX, IMIN, INC, IWGRBS, NB, NC
C     !COM
C---- ISOLA       as of 1997 Nov 19
      real*8      WAVEDEL
      common      /ISOLA/ WAVEDEL
C     Two Continuum Wavelength values are "equal" if their
C     relative difference is less than WAVEDEL.
C     .
C     !DASH
      external  NEARSD, COMPD, MOVE1, HI, BYE
      intrinsic max, min
C
C               XLMC(NC), XLMB(IWGRBS)
      dimension XLMC(*),  XLMB(*)
C
      call HI ('ROSATE')
C     !BEG
      INC = IWGRBS-1
      call NEARSD  (XLMC,NC,XLM, I)
      call COMPD   (XLM,XLMC(I),WAVEDEL,IFLG)
      if(IDIR.lt.0) then
        if(IFLG.gt.0) then
          IMAX = I
        else
          IMAX = I-1
        end if
        IMIN = max((IMAX-INC), 1)
      else
        if(IFLG.lt.0) then
          IMIN = I
        else
          IMIN = I+1
        end if
        IMAX = min((IMIN+INC),NC)
      end if
      if( ((IMAX-IMIN)+1) .eq. IWGRBS ) then
        NB = IWGRBS
        call MOVE1 (XLMC(IMIN),NB,XLMB)
      else
        NB = 0
      end if
C     !END
      call BYE ('ROSATE')
C
      return
      end
