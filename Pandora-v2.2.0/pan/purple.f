      subroutine PURPLE
     $(NO,N,K,IZ,Z,DL,SL,FL,SIG,BOT,TOP)
C
C     Rudolf Loeser, 1992 Mar 23
C---- Plots log(SLF) vs. DL, for YING.
C     (This is version 6 of PURPLE.)
C     !DASH
      save
C     !DASH
      real*8 BOT, DL, FL, SIG, SL, TOP, XL, XR, Z, ZS
      integer IZ, J, K, M, N, NH, NO, NV
      logical OK
      character NUMERO*1, PLUS*1
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
      external KINIT, KRIGIA, MOVED, SHRIMP, ABJECT, LINER, KPRINT,
     $         HI, BYE
C
C               IZ(5), Z(N), DL(K), SL(N,K), FL(K)
      dimension IZ(5), Z(*), DL(*), SL(N,*), FL(*)
C
      dimension ZS(5)
C
      data NV,NH /51, 117/
C     !EJECT
C
      call HI ('PURPLE')
C     !BEG
C---- Initialize plot image
      XL = DL(1)
      XR = DL(K)
      call KINIT    (IMAGE,XL,XR,BOT,TOP,NV,NH,NUMERO,OK)
      if(.not.OK) then
        call KRIGIA (XL,XR,BOT,TOP,NV,NH)
        go to 103
      end if
C---- Enter data
      do 100 J = 1,5
        M = IZ(J)
        ZS(J) = Z(M)
        call MOVED  (SL(M,1),N,K,FL,1,K)
        call SHRIMP (DL,K,1,K,FL,1,PLUS    ,1,SIG,2,IMAGE)
        call SHRIMP (DL,K,1,K,FL,1,ALPHS(J),1,SIG,1,IMAGE)
  100 continue
C---- Print
      call ABJECT   (NO)
      write (NO,101)
  101 format(' ','Plot of log(SLF) vs. Delta-Lambda')
      call LINER    (1,NO)
      call KPRINT   (IMAGE,NO)
      call LINER    (1,NO)
      write (NO,102) (ALPHS(J),ZS(J),J=1,5)
  102 format(' ',5(A1,' for Z =',1PE12.4,3X))
C
  103 continue
C     !END
      call BYE ('PURPLE')
C
      return
      end
