      subroutine MARTEL
     $(TAU,N,WTAB)
C
C     Rudolf Loeser, 1987 Jan 26
C---- "Reverses" a Tau table,
C     for the computation of back-face emergent spectrum.
C     (This is version 2 of MARTEL.)
C     !DASH
      save
C     !DASH
      real*8 TAU, TAUMAX, WTAB
      integer I, J, LUEO, N
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- MOSTAR      as of 2000 Sep 26
      real*8      COREWL,COREWN
      integer     ICORE
      character   WLAB1*10,WLAB2*2,WLAB3*12,WLAB4*10,WLAB5*2
      logical     WAVENO,SLINE,WHOLE,RED,BLUE
      common      /MOSTAR1/ COREWL,COREWN
      common      /MOSTAR2/ ICORE
      common      /MOSTAR3/ WLAB1,WLAB2,WLAB3,WLAB4,WLAB5
      common      /MOSTAR4/ WAVENO,SLINE,WHOLE,RED,BLUE
C     Wavelength/Wavenumber print/plot controls (subroutine WUMBLE).
C     .
C     !DASH
C     !EJECT
      external REVERSD, MINUSD, IS_INCREASING, MESHED, ABORT, VECOUT,
     $         HI, BYE
C
C               TAU(N)
      dimension TAU(*)
C
      call HI ('MARTEL')
C     !BEG
      if(N.gt.1) then
        TAUMAX = TAU(N)
        do 100 I = 1,N
          TAU(I) = TAUMAX-TAU(I)
  100   continue
        call REVERSD       (TAU, 1, N)
C
        call IS_INCREASING (TAU, 1, N, 0, J)
        if(J.le.0) then
          call MINUSD      (TAU, 1, N, J)
        end if
C
        if(J.gt.0) then
          call MESHED      ('MARTEL', 1)
          write (LUEO,101) WLAB1,WTAB
  101     format(' ','Unusable reversed Tau for ',A,1X,1PE20.12)
          call VECOUT      (LUEO, TAU, N, 'TAU')
          call ABORT
        end if
      end if
C     !END
      call BYE ('MARTEL')
C
      return
      end
