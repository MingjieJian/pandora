      subroutine PANSA
     $(XLAM,YNT,MUX,Y,MYX,BRIGHT,XLTIT,ISTAR,MODE,EXPAND)
C
C     Rudolf Loeser, 1986 Nov 06
C---- Saves Spectrum Summary data.
C     (This is version 3 of PANSA.)
C     !DASH
      save
C     !DASH
      real*8 BRIGHT, XLAM, XLTIT, Y, YNT
      integer IPEX, ISTAR, LUEO, MODE, MUX, MYX
      logical EXPAND, OK
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
C     !EJECT
      external PILOT, CEBU, MESHED, MASHED, HI, BYE
C
      call HI ('PANSA')
C     !BEG
      if(ICSTRT) then
C----   Initialize
        ICSTRT = .false.
        ICFULL = .false.
        OK     = .true.
        NIADR  = 1
        NICON  = 0
        SSBUFF(NICON+1) = ZZLARGE
      end if
      if((IPEX.lt.0).or.(IPEX.eq.7)) then
        call MESHED     ('PANSA', 2)
        write (LUEO,100) NIADR,NICON,XLAM,YNT,MUX,MYX,Y,BRIGHT,
     $                   XLTIT,ISTAR,MODE,EXPAND
  100   format(' ',I6,I4,1P2E12.4,2I4,3E12.4,I6,I4,L3)
        call MASHED     ('PANSA')
      end if
C
      if(OK) then
C----   Enter data into buffer, and add end-of-data flag
        call PILOT      (1, SSBUFF(NICON+1), XLAM, YNT, MUX, MYX, Y,
     $                   BRIGHT, XLTIT, ISTAR, MODE, EXPAND)
        NICON = NICON+ICBNCH
        SSBUFF(NICON+1) = ZZLARGE
C
        if((NICON+1).gt.MXICON) then
C----     Buffer is full; try to write it and save the record address
          call CEBU     (SSBUFF, MXICON, ICADRS(NIADR))
          NIADR = NIADR+1
          NICON = 0
          SSBUFF(NICON+1) = ZZLARGE
C
          if(NIADR.gt.MXIADR) then
C----       No more room for record addresses - disable further data
C           collection
            OK     = .false.
            ICFULL = .true.
C
            call MESHED ('PANSA', 3)
            write (LUEO,101) MXIADR
  101       format(' ','Room for Spectrum Summary data has been ',
     $                 'exhausted (MXIADR =',I6,')'/
     $             ' ','"SPECTRUM" and "CONTRIBUTORS" analyses will ',
     $                 'be skipped.')
            call MASHED ('PANSA')
          end if
        end if
      end if
C     !END
      call BYE ('PANSA')
C
      return
      end
