      subroutine ZORRO
     $(XLAM,YNT,MUX,Y,MYX,BRIGHT,XLTIT,ISTAR,MODE,EXPAND,KEOF)
C
C     Rudolf Loeser, 1986 Nov 06
C---- Retrieves Spectrum Summary data.
C     (This is version 4 of ZORRO.)
C     !DASH
      save
C     !DASH
      real*8 BRIGHT, XLAM, XLTIT, Y, YNT
      integer IPEX, ISTAR, KEOF, LUEO, MODE, MUX, MYX, NCNSV
      logical EXPAND
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 18),IPEX )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- ULTIMA      as of 2004 Mar 09
      real*8      ZZLARGE,ZZSMALL,ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
      common      /ULTIMA/ ZZLARGE,ZZSMALL,
     $                     ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
C     Extreme values of floating point numbers range.
C     (See also subroutines SNAFU, SNUFFLE and FOOZLE.)
C     .
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
      external CEBU, LEYTE, PILOT, MESHED, MASHED, HI, BYE
C     !EJECT
C
      call HI ('ZORRO')
C     !BEG
      if(ICSTRT) then
C----   Initialize
        ICSTRT = .false.
C----   Flush current (final) buffer contents
        call CEBU   (SSBUFF, MXICON, ICADRS(NIADR))
        KEOF  = 0
        NIADR = 0
        NICON = MXICON
      end if
C
      if((NICON+1).gt.MXICON) then
C----   Read next record into buffer
        NIADR = NIADR+1
        call LEYTE  (SSBUFF, MXICON, ICADRS(NIADR))
        NICON = 0
      end if
C
C---- Retrieve data
      call PILOT    (2, SSBUFF(NICON+1), XLAM, YNT, MUX, MYX, Y,
     $               BRIGHT, XLTIT, ISTAR, MODE, EXPAND)
      NCNSV = NICON
      NICON = NICON+ICBNCH
      if(XLAM.eq.ZZLARGE) then
C----   This is the end-of-data signal - set indicators
        KEOF  = 1
        NICON = 0
      end if
C
      if((IPEX.lt.0).or.(IPEX.eq.7)) then
        call MESHED ('ZORRO', 2)
        write (LUEO,100) NIADR,NCNSV,XLAM,YNT,MUX,MYX,Y,BRIGHT,XLTIT,
     $                   ISTAR,MODE,EXPAND,ICADRS(NIADR),KEOF
  100   format(' ','ZORRO',I6,I4,1P2E12.4,2I4,3E12.4,I6,I4,L4,I12,I4)
        call MASHED ('ZORRO')
      end if
C     !END
      call BYE ('ZORRO')
C
      return
      end
