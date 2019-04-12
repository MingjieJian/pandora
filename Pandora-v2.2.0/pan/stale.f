      subroutine STALE
C
C     Rudolf Loeser, 1994 Jan 27
C---- Dumps ICON, for SWALE.
C     !DASH
      save
C     !DASH
      integer I, IPEX, LUEO
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
      external MESHED, LINER, VECOUT, MASHED, HI, BYE
C     !EJECT
C
      call HI ('STALE')
C     !BEG
      if(IPEX.eq.7) then
        call MESHED ('STALE', 2)
        write (LUEO,100) ICSTRT,ICFULL,NIADR,NICON,ICBNCH,MXICON,MXIADR
  100   format(' ','Dump of ICON: Spectrum Summary Data collection ',
     $             'apparatus.',5X,'ICSTRT =',L2,5X,'ICFULL =',L2//
     $         ' ','NIADR =',I10,5X,'nicon =',I10,5X,'ICBNCH =',I10,5X,
     $             'MXICON =',I10,5X,'MXIADR =',I10)
C
        call LINER  (1, LUEO)
        write (LUEO,101)
  101   format(' ','ICADRS')
        call LINER  (1, LUEO)
        write (LUEO,102) (ICADRS(I),I=1,NIADR)
  102   format(' ',5I12,7X,5I12)
C
        call VECOUT (LUEO, SSBUFF, NICON, 'SSBUFF')
        call MASHED ('STALE')
      end if
C     !END
      call BYE ('STALE')
C
      return
      end
