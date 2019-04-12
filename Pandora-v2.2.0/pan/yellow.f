      subroutine YELLOW
     $(NO,N,K,ID,DL,Z,ZL,SL,FL,SIG,BOT,TOP)
C
C     Rudolf Loeser, 1992 Mar 23
C---- Plots log(SLF) vs. Z, for YING.
C     (This is version 5 of YELLOW.)
C     !DASH
      save
C     !DASH
      real*8 BOT, DL, DS, FL, SIG, SL, TOP, XL, XR, Z, ZL, dummy
      integer IBEG, ID, IEND, J, K, M, N, NH, NO, NV
      logical OK
      character NUMERO*1, PLUS*1, TIT*10
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(38),NUMERO)
      equivalence (SYMBS(39),PLUS  )
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
      external ZED, KINIT, KRIGIA, MOVE1, SHRIMP, ABJECT, LINER, KPRINT,
     $         HI, BYE
C
C               ID(5), Z(N), ZL(N), SL(N,K), DL(K), FL(N)
      dimension ID(5), Z(*), ZL(*), SL(N,*), DL(K), FL(*)
C
      dimension DS(5)
C
      data NV,NH /51, 117/
C     !EJECT
C
      call HI ('YELLOW')
C     !BEG
C---- Establish ordinate
      IBEG = 0
      IEND = 0
      call ZED      (Z,N,dummy,0, 1,IBEG,IEND,ZL,TIT,'YELLOW')
      XL = ZL(IBEG)
      XR = ZL(IEND)
C---- Initialize plot image
      call KINIT    (IMAGE,XL,XR,BOT,TOP,NV,NH,NUMERO,OK)
      if(.not.OK) then
        call KRIGIA (XL,XR,BOT,TOP,NV,NH)
        go to 103
      end if
C
C---- Enter data
      do 100 J = 1,5
        M = ID(J)
        DS(J)= DL(M)
        call MOVE1  (SL(1,M),N,FL)
        call SHRIMP (ZL,N,IBEG,IEND,FL,1,PLUS    ,1,SIG,2,IMAGE)
        call SHRIMP (ZL,N,IBEG,IEND,FL,1,ALPHS(J),1,SIG,1,IMAGE)
  100 continue
C
C---- Print
      call ABJECT   (NO)
      write (NO,101) TIT
  101 format(' ','Plot of log(SLF) vs. ',A10)
      call LINER    (1,NO)
      call KPRINT   (IMAGE,NO)
      call LINER    (1,NO)
      write (NO,102) (ALPHS(J),DS(J),J=1,5)
  102 format(' ',5(A1,' for DL =',1PE12.4,3X))
C
  103 continue
C     !END
      call BYE ('YELLOW')
C
      return
      end
