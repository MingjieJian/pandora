      subroutine LOX
     $(PLANE)
C
C     Rudolf Loeser, 1986 Dec 11
C---- Initializes Spectrum Summary data collection process.
C     (This is version 3 of LOX.)
C     !DASH
      save
C     !DASH
      logical PLANE
C     !COM
C---- ICON        as of 1999 Mar 30
      integer     ICBNCH,MXICON,MXIADR
      parameter   (ICBNCH=10)
      parameter   (MXICON=50*ICBNCH)
      parameter   (MXIADR=1000)
C     (Remember to recompile all users when changing any parameter!)
      integer     ICADRS,NIADR,NICON
      real*8      SSBUFF
      logical     ICSTRT, ICFULL
      dimension   ICADRS(MXIADR),SSBUFF(MXICON+ICBNCH)
      common      /ICON1/ NICON,NIADR,ICADRS
      common      /ICON2/ SSBUFF
      common      /ICON3/ ICSTRT, ICFULL
C     Buffer, and record addresses, and control parameters,
C     for saving/restoring Spectrum Summary data.
C     .
C     !DASH
      external HI, BYE
C
      call HI ('LOX')
C     !BEG
      if(PLANE) then
        ICSTRT = .true.
      end if
C     !END
      call BYE ('LOX')
C
      return
      end
