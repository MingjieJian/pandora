      subroutine REAP
     $(M,MUX,MYX,XLAMB,BRIGHT,FRACT,YNT,XLTIT,ISTAR,MODE,LFB,N)
C
C     Rudolf Loeser, 1973 Apr 04
C---- Reads Spectrum Summary data.
C     !DASH
      save
C     !DASH
      real*8 B, BRIGHT, F, FRACT, XL, XLAMB, XLT, XLTIT, Y, YNT
      integer IST, ISTAR, JFB, JST, KEOF, LFB, M, MD, MODE, MUX, MX, MY,
     $        MYX, N, NP
      logical lummy
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
      external ZORRO, HI, BYE
C
C               XLTIT(NW), XLAMB(NW), BRIGHT(NW), FRACT(NW), ISTAR(NW),
      dimension XLTIT(*),  XLAMB(*),  BRIGHT(*),  FRACT(*),  ISTAR(*),
C
C               MODE(NW), MYX(NW), MUX(NW), YNT(NW)
     $          MODE(*),  MYX(*),  MUX(*),  YNT(*)
C     !EJECT
C
      call HI ('REAP')
C     !BEG
      M  = 0
      NP = N+1
      ICSTRT = .true.
  100 continue
C
        call ZORRO (XL,Y,MX,F,MY,B,XLT,JST,MD,lummy,KEOF)
C
        if(KEOF.ne.1) then
          JFB = JST/100
          IST = JST-100*JFB
          if(JFB.eq.LFB) then
C
            M = M+1
            YNT(M) = Y
            MUX(M) = 0
            MYX(M) = 0
            XLAMB(M) = XL
            if(JFB.eq.1) then
              MUX(M) = MX
              MYX(M) = MY
            else if(JFB.eq.2) then
              MUX(M) = NP-MX
              MYX(M) = NP-MY
            end if
            MODE(M)   = MD
            FRACT(M)  = F
            XLTIT(M)  = XLT
            ISTAR(M)  = IST
            BRIGHT(M) = B
C
          end if
          go to 100
        end if
C
      ICSTRT = .true.
C     !END
      call BYE ('REAP')
C
      return
      end
