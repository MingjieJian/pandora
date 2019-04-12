      subroutine ZEBRA
     $(Z,ZL,N,K,IBEG,IEND,POINTS,SIG,XMODU,IMAGE,TIT,CALLER)
C
C     Rudolf Loeser, 1968 Oct 18
C---- Sets up plots vs. Z.
C     !DASH
      save
C     !DASH
      real*8 BOT, POINTS, SIG, TEN, TOP, XL, XMODU, XR, YLL, YUL, Z,
     $       ZERO, ZL, dummy
      integer IBEG, IEND, IPEX, J, K, KNT, KOPT, LUEO, N, NH, NV
      logical GOOD
      character CALLER*8, IMAGE*(*), NUMERO*1, PERIOD*1, TIT*10
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
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(11),TEN   )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(38),NUMERO)
      equivalence (SYMBS(42),PERIOD)
C     !DASH
C     !EJECT
      external CHROME, BELOWD, ABOVED, MESHED, KRIGIA, MONKEY, KLINEC,
     $         ZED, KINIT, MASHED, HI, BYE
C
C               Z(N), ZL(N), POINTS(N,K)
      dimension Z(*), ZL(*), POINTS(N,*)
C
      data NV,NH,KOPT /51, 117, 0/
C
      call HI ('ZEBRA')
C     !BEG
      if((IPEX.lt.0).or.(IPEX.eq.19)) then
        call MESHED ('ZEBRA', 2)
        write (LUEO,100) CALLER
  100   format(' ','ZEBRA',2X,'from ',A8)
        call MASHED ('ZEBRA')
      end if
C
C---- Set up Z points
      IBEG = 0
      IEND = 0
      call ZED      (Z, N, dummy, 0, KOPT, IBEG, IEND, ZL, TIT, 'ZEBRA')
      XL = ZL(IBEG)
      XR = ZL(IEND)
C---- Establish ordinate limits
      KNT = IEND-(IBEG-1)
      YUL = -ZZLARGE
      YLL = +ZZLARGE
      do 101 J = 1,K
        call CHROME (KNT, 1, POINTS(IBEG,J), 1, SIG, YUL, YLL)
  101 continue
      call BELOWD   (YLL, XMODU, BOT)
      call ABOVED   (YUL, XMODU, TOP)
C---- Initialize plot image
      call KINIT    (IMAGE, XL, XR, BOT, TOP, NV, NH, NUMERO, GOOD)
      if(.not.GOOD) then
        call KRIGIA (XL, XR, BOT, TOP, NV, NH)
      end if
C
      if((BOT.lt.ZERO).and.(TOP.gt.ZERO)) then
C----   Enter reference line at Y=0
        call KLINEC (IMAGE, XL, ZERO, XR, ZERO, PERIOD, 0)
      end if
      if(TIT.eq.'Z-index') then
C----   Enter grid lines
        call MONKEY (IMAGE, XL, XR, TEN, BOT, TOP, PERIOD, 1)
      end if
C     !END
      call BYE ('ZEBRA')
C
      return
      end
