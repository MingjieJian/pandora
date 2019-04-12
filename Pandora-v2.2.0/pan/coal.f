      subroutine COAL
     $(NO,TE,TR,LIM,Z,N,ZLOG,LAB,REMARK,LEVELS)
C
C     Rudolf Loeser, 1973 Apr 03
C---- Plots TR and TE vs. Z
C     (This is version 2 of COAL.)
C     !DASH
      save
C     !DASH
      real*8 TE, TINT, TR, TRMAX, Z, ZLOG
      integer IBEG, IEND, IMAX, J, LIM, N, NO, jummy
      character LAB*(*), LEVELS*20, REMARK*8, STAR*1, TIT*10
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(45),STAR  )
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
      external  LACO, ZEBRA, SHRIMP, ABJECT, LINER, KPRINT, BOUNDLO,
     $          MINMAXD, HI, BYE
      intrinsic max
C
C               TR(N,LIM), Z(N), ZLOG(N), TE(N)
      dimension TR(N,*),   Z(*), ZLOG(*), TE(*)
C     !EJECT
C
      call HI ('COAL')
C     !BEG
C---- Edit data, and determine max
      TRMAX = -ZZLARGE
      do 100 J = 1,LIM
        call BOUNDLO (N,TR(1,J),ZZSMALL)
        call MINMAXD (TR(1,J),1,N,jummy,IMAX)
        TRMAX = max(TRMAX,TR(IMAX,J))
  100 continue
C
      if(TRMAX.gt.ZZSMALL) then
C----   Compute rounding parameter
        call LACO    (TRMAX,TINT)
C----   Initialize plot image
        call ZEBRA   (Z,ZLOG,N,LIM,IBEG,IEND,TR,ZZSMALL,TINT,IMAGE,
     $                TIT,'COAL')
C
C----   Enter points into image
        call SHRIMP  (ZLOG,N,IBEG,IEND,TR,LIM,ALPHS,26,ZZSMALL,2,IMAGE)
        call SHRIMP  (ZLOG,N,IBEG,IEND,TE,  1,STAR , 1,ZZSMALL,2,IMAGE)
C
C----   Write graph header and image
        call ABJECT  (NO)
        write (NO,101) LAB,TIT,REMARK,LEVELS
  101   format(' ','Graph of ',A,' TR of each level vs. ',A10,10X,A8,
     $             ' graph ',A20)
        call LINER   (1,NO)
        call KPRINT  (IMAGE,NO)
      end if
C     !END
      call BYE ('COAL')
C
      return
      end
