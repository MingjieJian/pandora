      subroutine SAPHIRE
     $(W,WVL,K,DL,KFUNC,AHZ,EW,BT,FIN,FAN,FIS,XLA,VEC)
C
C     Rudolf Loeser, 1977 Jan 28
C---- Computes auxiliary functions for spectrum printouts.
C     If KFUNC = 1, then input AHZ is Intensity/Hz,
C        KFUNC = 2,            AHZ is Flux/Hz.
C
C---- Output is:
C     EW  - equivalent width, in Angstroms,
C     BT  - brightness temperature, in Kelvins,
C     FIN - integrated AHZ, /Hz,
C     FAN - AHZ in units of /A,
C     FIS - line without continuum, and
C     XLA - luminosity (from Flux only).
C     !DASH
      save
C     !DASH
      real*8 AHZ, BT, DL, EW, FAN, FIN, FIS, R1N, VEC, W, WVL, XLA, XLM
      integer I, K, KFUNC
      logical FLX
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (RZQ( 23),R1N  )
C     !DASH
      external TATIAN, HALT, ONYX, OCEAN, PLAY, UTHER, MONA, CASPIA,
     $         HI, BYE
C
      dimension W(*)
C
C               FIS(KM), VEC(KM), FAN(KM), AHZ(KM), FIN(KM), XLA(KM),
      dimension FIS(*),  VEC(*),  FAN(*),  AHZ(*),  FIN(*),  XLA(*),
C
C               DL(KM), BT(KM), EW(KM)
     $          DL(*),  BT(*),  EW(*)
C     !EJECT
C
      call HI ('SAPHIRE')
C     !BEG
      if((KFUNC.lt.1).or.(KFUNC.gt.2)) then
        write (MSSLIN(1),100) KFUNC
  100   format('KFUNC =',I12,', which is neither 1 nor 2.')
        call HALT   ('SAPHIRE', 1)
      end if
      FLX = KFUNC.eq.2
C
      call CASPIA   (KFUNC, K, AHZ, VEC)
      call TATIAN   (W, DL, VEC, K, EW)
C
      do 101 I = 1,K
        call ONYX   (WVL, DL(I), EW(I), AHZ(I), FIN(I))
        XLM = WVL+DL(I)
        call OCEAN  (XLM, VEC(I), BT(I))
        call PLAY   (XLM, AHZ(I), FAN(I))
        call UTHER  (FIN(I), FAN(I), DL(I), FIS(I))
        if(FLX) then
          call MONA (WVL, R1N, VEC(I), XLA(I))
        end if
  101 continue
C     !END
      call BYE ('SAPHIRE')
C
      return
      end
