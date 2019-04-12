      subroutine CANARY
     $(NO,DL,K,PHI,ZR,NR)
C
C     Rudolf Loeser, 1978 Jul 26
C---- Plots, for PEEK.
C     !DASH
      save
C     !DASH
      real*8 DL, PHI, SIX, ZERO, ZR
      integer I, K, NF, NH, NO, NR, NV
      logical GOOD, SWITCH
      character NUMERO*1, PERIOD*1, ZC*10
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 7),SIX   )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(38),NUMERO)
      equivalence (SYMBS(42),PERIOD)
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
      external  KINIT, KLINEC, ABJECT, LINER, RACCOON, KPRINT, ENCODED,
     $          KRIGIA, HI, BYE
C
C               DL(KM), PHI(Nx ,KM), ZR(N)
      dimension DL(*),  PHI(*),      ZR(*)
C
      dimension ZC(10)
C
      data NV,NH /55, 117/
      data SWITCH /.true./
C     !EJECT
C
      call HI ('CANARY')
C     !BEG
      if(NO.gt.0) then
C----   Initialize image
        call KINIT     (IMAGE,DL(1),DL(K),-SIX,ZERO,NV,NH,NUMERO,GOOD)
        if(.not.GOOD) then
          call KRIGIA  (DL(1),DL(K),-SIX,ZERO,NV,NH)
        end if
        if((DL(1).lt.ZERO).AND.(DL(K).gt.ZERO)) then
          call KLINEC  (IMAGE,ZERO,-SIX,ZERO,ZERO,PERIOD,0)
        end if
C----   Enter data
        call RACCOON   (K,DL,NR,PHI,SWITCH)
C----   Print
        call ABJECT    (NO)
        write (NO,100)
  100   format(' ','Graph of log(PHI) vs. DL, for selected depths.')
        call LINER     (1,NO)
        call KPRINT    (IMAGE,NO)
C----   Devise and print legend
        do 101 I = 1,NR
          call ENCODED (ZR(I),ZC(I),10,7,1,NF)
  101   continue
        write (NO,102) (ALPHS(I),ZC(I),I=1,NR)
  102   format(5(' Curve ',A1,': Z=',A10,:,3X))
      end if
C     !END
      call BYE ('CANARY')
C
      return
      end
