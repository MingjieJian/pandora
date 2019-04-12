      subroutine ACTON
     $(N,DEE,DEEL,REM)
C
C     Rudolf Loeser, 1998 Oct 30
C---- Plots d-coefficients (diffusion).
C     !DASH
      save
C     !DASH
      real*8 DEE, DEEL, TEN, YBOT, YTOP, ZMAX, ZMIN
      integer I, J, JQ, JX, L, LUEO, N, NH, NV
      logical OK
      character LAB*3, LABEL*30, NUMERO*1, PERIOD*1, REM*(*), SYM*1
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(11),TEN   )
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
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
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
C     !EJECT
      external CONTA, MONKEY, NOTAC, LOGO, KPRINT, GABOR, KINIT, LINER,
     $         ABJECT, KRIGIA, HI, BYE
C
C               DEE(4,5,N), DEEL(4,5,N)
      dimension DEE(*),     DEEL(*)
C
      dimension LAB(4,5)
C
      data NV,NH /51, 117/
      data LABEL /'Plot of logs of d-coefficients'/
C
      call HI ('ACTON')
C     !BEG
C---- Get logs, of abs(d) > 0
      call LOGO        (DEE, (20*N), 0, ZL10SMA, DEEL)
C---- Get plot limits
      call CONTA       (N, DEE, DEEL, JQ, JX, YBOT, YTOP)
      if((JX-JQ).gt.10) then
C
C----   Initialize plot image
        ZMIN = JQ
        ZMAX = JX
        call KINIT     (IMAGE, ZMIN, ZMAX, YBOT, YTOP, NV, NH, NUMERO,
     $                  OK)
        if(.not.OK) then
          call KRIGIA  (ZMIN, ZMAX, YBOT, YTOP, NV, NH)
        end if
        call MONKEY    (IMAGE, ZMIN, ZMAX, TEN, YBOT, YTOP, PERIOD, 1)
C----   Enter data
        L = 0
        do 101 I = 1,4
          do 100 J = 1,5
            L = L+1
            SYM = ALPHS(L)
            call NOTAC (IMAGE, JQ, JX, I, J, DEE, DEEL, LAB(I,J), SYM)
  100     continue
  101   continue
C----   Print plot
        call ABJECT (LUEO)
        write (LUEO,102) LABEL,REM
  102   format(' ',A,' vs. Z-index (lower case for negative values) ',
     $            A,'.')
        call LINER     (1, LUEO)
        call KPRINT    (IMAGE, LUEO)
        do 104 I = 1,4
          write (LUEO,103) (I,J,LAB(I,J),J=1,5)
  103     format(' ',10X,5('d(',I1,',',I1,') = ',A3,3X))
  104   continue
C
      else
C----   So there!
        call GABOR     (LUEO, 3, 0, LABEL)
      end if
C     !END
      call BYE ('ACTON')
C
      return
      end
