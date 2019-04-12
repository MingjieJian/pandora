      subroutine NAMMU
     $(N,NL,NO,LCX,CXX,CXXP,G,LISTXL,ARR,Z,ZL)
C
C     Rudolf Loeser, 1990 Dec 03
C---- Plots arrays of upper-level charge exchange data, for KILOSA.
C     !DASH
      save
C     !DASH
      real*8 ARR, CXX, CXXP, G, Z, ZL
      integer LCX, LISTXL, LL, N, NL, NO
C     !COM
C---- ULTIMA      as of 2004 Mar 09
      real*8      ZZLARGE,ZZSMALL,ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
      common      /ULTIMA/ ZZLARGE,ZZSMALL,
     $                     ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
C     Extreme values of floating point numbers range.
C     (See also subroutines SNAFU, SNUFFLE and FOOZLE.)
C     .
C     !DASH
      external  HULU, MIKKU, KIDAL, HI, BYE
      intrinsic min
C
C               ARR(N,LL), CXX(N,NL), CXXP(N,NL), G(N,NL), Z(N), ZL(N),
      dimension ARR(*),    CXX(*),    CXXP(*),    G(*),    Z(*), ZL(*),
C
C               LISTXL(NL), LCX(NL)
     $          LISTXL(*),  LCX(*)
C
      call HI ('NAMMU')
C     !BEG
C---- Make list of "charge exchange levels"
      call HULU    (NL, LCX, LL, LISTXL)
      LL = min(LL,26)
C
      if((LL.gt.0).and.(NO.gt.0)) then
C----   G
        call MIKKU (LL, LISTXL, N, NL, G,    ARR, ZZSMALL, 0)
        call KIDAL (N, LL, ARR, Z, ZL, LISTXL, NO,
     $              'Charge Transfer Coefficient G')
C----   CXXP
        call MIKKU (LL, LISTXL, N, NL, CXXP, ARR, ZZSMALL, 1)
        call KIDAL (N, LL, ARR, Z, ZL, LISTXL, NO,
     $              'log( Ion-to-neutral Rate XP )')
C----   CXX
        call MIKKU (LL, LISTXL, N, NL, CXX,  ARR, ZZSMALL, 1)
        call KIDAL (N, LL, ARR, Z, ZL, LISTXL, NO,
     $              'log( Neutral-to-ion Rate X )')
      end if
C     !END
      call BYE ('NAMMU')
C
      return
      end
