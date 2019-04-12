      subroutine JARED
     $(NO,N,K,Z,ZL,SL,FL,SIG,BOT,TOP)
C
C     Rudolf Loeser, 1992 Dec 31
C---- Plots all log(SLF) vs. Z, for YING.
C     !DASH
      save
C     !DASH
      real*8 BOT, FL, SIG, SL, TOP, Z, ZL, dummy
      integer IBEG, IEND, J, K, L, N, NH, NO, NV
      logical OK
      character NUMERO*1, TIT*10
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(38),NUMERO)
C
C---- IMAGE       as of 1997 Aug 21
      integer     IMALEN
      parameter   (IMALEN=65535)
      character   IMAGE*(IMALEN)
      common      /IMAGE/ IMAGE
C     Character string to hold plot images constructed by the
C     K-type line printer plotting routines;
C     but used also as a general scratch character array.
C     .
C     !DASH
      external  KINIT, KRIGIA, MOVE1, SHRIMP, ABJECT, LINER, KPRINT,
     $          ZED, HI, BYE
      intrinsic mod
C
C               Z(N), ZL(N), SL(N,K), FL(N)
      dimension Z(*), ZL(*), SL(N,*), FL(*)
C
      data NV,NH /51, 117/
C     !EJECT
C
      call HI ('JARED')
C     !BEG
C---- Establish ordinate
      IBEG = 0
      IEND = 0
      call ZED      (Z,N,dummy,0, 1,IBEG,IEND,ZL,TIT,'JARED')
C---- Initialize plot image
      call KINIT    (IMAGE,ZL(IBEG),ZL(IEND),BOT,TOP,NV,NH,NUMERO,OK)
      if(.not.OK) then
        call KRIGIA (ZL(IBEG),ZL(IEND),BOT,TOP,NV,NH)
        go to 102
      end if
C
C---- Enter data
      do 100 J = 1,K
        call MOVE1  (SL(1,J),N,FL)
        L = mod((J-1),26)+1
        call SHRIMP (ZL,N,IBEG,IEND,FL,1,ALPHS(L),1,SIG,2,IMAGE)
  100 continue
C
C---- Print
      call ABJECT   (NO)
      write (NO,101) TIT,K
  101 format(' ','Plot of log(SLF) vs. ',A10,' for all DL; K =',I3,'.'/
     $       ' ','            SLF is the frequency-dependent line ',
     $           'source function, at every wavelength across the ',
     $           'profile.')
      call LINER    (1,NO)
      call KPRINT   (IMAGE,NO)
C
  102 continue
C     !END
      call BYE ('JARED')
C
      return
      end
