      subroutine HALIBUT
     $(MUX,TE,BRIGHT,N,NW,IMAGE)
C
C     Rudolf Loeser, 1980 Dev 11
C---- Initializes spectrum summary plot image, for FOUND.
C     !DASH
      save
C     !DASH
      real*8 BRIGHT, CWARTR, RX1, RX2, RX3, RX4, RX5, TE, TEMUX, TEMYN,
     $       TEN, TINT, XMUX, XMYN, YL, YMUX, YMYN, ZERO
      integer IMUX, IMYN, IPEX, ITB, JMUX, JMYN, KTE, LUEO, MIOW, MMUX,
     $        MMYN, MUX, N, NH, NV, NW
      logical GOOD
      character IMAGE*(*), NUMERO*1
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
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(14),CWARTR)
      equivalence (DLIT(11),TEN   )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(38),NUMERO)
C     !DASH
C     !EJECT
      external  MESHED, MINMAXD, MASHED, BELOWD, ABOVED, KRIGIA, KINIT,
     $          MINMAXI, HI, BYE
      intrinsic min, max
C
C               TE(N), BRIGHT(Nmkuse), MUX(Nmkuse)
      dimension TE(*), BRIGHT(*),  MUX(*)
C
      data RX1,RX2,RX3 /1.25D0, 1.1D0, 9.D-1/
      data RX4,RX5 /1.000000001D0, 9.99999999D-1/
      data NV,NH /54, 117/
C
      call HI ('HALIBUT')
C     !BEG
C---- Establish ordinate limits
      call MINMAXI  (MUX, 1, NW, MMYN, MMUX)
      ITB = MMYN-1
      KTE = MMUX-ITB
      call MINMAXD  (TE(ITB+1), 1, KTE, IMYN, IMUX)
      TEMYN = TE(ITB+IMYN)
      TEMUX = TE(ITB+IMUX)
      call MINMAXD  (BRIGHT, 1, NW, JMYN, JMUX)
      YMUX = max(BRIGHT(JMUX)*RX1,TEMYN)
      YMYN = min(BRIGHT(JMYN)/RX1,TEMUX)
      if(YMUX.eq.YMYN) then
        YMUX = RX2*YMUX
        YMYN = RX3*YMYN
      end if
      YL   = log10(YMUX)
      MIOW = YL
      TINT = TEN**MIOW
      TINT = CWARTR*TINT
      call BELOWD   ((RX4*YMYN), TINT, YMYN)
      call ABOVED   ((RX5*YMUX), TINT, YMUX)
C---- Initialize plot image
      XMYN = ZERO
      XMUX = N+1
      call KINIT    (IMAGE, XMYN, XMUX, YMYN, YMUX, NV, NH, NUMERO,
     $               GOOD)
      if(.not.GOOD) then
        call KRIGIA (XMYN, XMUX, YMYN, YMUX, NV, NH)
      end if
C
      if((IPEX.lt.0).or.(IPEX.eq.7)) then
        call MESHED ('HALIBUT', 2)
        write (LUEO,100) N,NW,MMYN,MMUX,ITB,KTE,IMYN,IMUX,TEMYN,TEMUX,
     $                   JMYN,JMUX,BRIGHT(JMUX),BRIGHT(JMYN),
     $                   MIOW,TINT,YMYN,YMUX,XMYN,XMUX
  100   format(' ',8I10,1P2E20.12/
     $         ' ',2I10,2E20.12/
     $         ' ',I10,5E20.12)
        call MASHED ('HALIBUT')
      end if
C     !END
      call BYE ('HALIBUT')
C
      return
      end
