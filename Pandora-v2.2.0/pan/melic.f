      subroutine MELIC
     $(IMAGE,XL,XU,YL,YU)
C
C     Rudolf Loeser, 1978 May 01
C---- Sets up an Emission Profile graph.
C     !DASH
      save
C     !DASH
      real*8 XL, XU, YL, YU, ZERO
      character IMAGE*(*), PERIOD*1
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(42),PERIOD)
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
      external ANDREW, KLINEC, HI, BYE
C
      call HI ('MELIC')
C     !BEG
      call ANDREW     (IMAGE,XL,XU,YL,YU)
C
      if(WHOLE) then
        if(WAVENO) then
          call KLINEC (IMAGE,COREWN,YL,COREWN,YU,PERIOD,0)
        else
          call KLINEC (IMAGE,ZERO  ,YL,ZERO  ,YU,PERIOD,0)
        end if
      end if
C     !END
      call BYE ('MELIC')
C
      return
      end
