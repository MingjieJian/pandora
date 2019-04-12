      subroutine HORN
     $(A,N,X,SA,NDT,LU)
C
C     Rudolf Loeser, 1973 Oct 24
C---- Makes frequency graphs, for SHAFT.
C     !DASH
      save
C     !DASH
      real*8 A, SA, X, XH, XL, YH, YL
      integer I, IM, K, LU, N, NDT, NH, NV
      logical GOOD
      character NUMERO*1, SYM*1
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(38),NUMERO)
C
C---- ULTIMA      as of 2004 Mar 09
      real*8      ZZLARGE,ZZSMALL,ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
      common      /ULTIMA/ ZZLARGE,ZZSMALL,
     $                     ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
C     Extreme values of floating point numbers range.
C     (See also subroutines SNAFU, SNUFFLE and FOOZLE.)
C     .
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
      external BOW, KINIT, SHRIMP, MOVED, KRIGIA, LINER, KPRINT, HI, BYE
C
C               A(N,NDT), X(NDT), SA(NDT,3)
      dimension A(N,*),   X(*),   SA(NDT,*)
C
      dimension IM(3), SYM(3)
C
      data NV,NH /56,117/
      data SYM /'A', '*', 'Z'/
C     !EJECT
C
      call HI ('HORN')
C     !BEG
      if(LU.gt.0) then
        IM(1) = 1
        IM(2) = N/2
        IM(3) = N
C
        do 100 K = 1,3
          I = IM(K)
          call MOVED  (A(I,1),N,NDT,SA(1,I),1,NDT)
  100   continue
C
        call BOW      (3,X,NDT,SA,ZZSMALL,XL,XH,YL,YH)
C
        call KINIT    (IMAGE,XL,XH,YL,YH,NV,NH,NUMERO,GOOD)
        if(.not.GOOD) then
          call KRIGIA (XL,XH,YL,YH,NV,NH)
        end if
C
        call SHRIMP   (X,NDT,1,NDT,SA,3,SYM,3,ZZSMALL,2,IMAGE)
C
        call KPRINT   (IMAGE,LU)
        call LINER    (1,LU)
        write (LU,101) SYM
  101   format(' ',A1,': first depth;',5X,A1,': middle depth;',5X,
     $             A1,': last depth.')
      end if
C     !END
      call BYE ('HORN')
C
      return
      end
