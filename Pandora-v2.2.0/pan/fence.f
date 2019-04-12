      subroutine FENCE
     $(LU,M,TE,HND,H1,HK,XNE,HE1,HE2,HE3,XX,FL,KODE)
C
C     Rudolf Loeser, 1997 Mar 25
C---- Plots, for GARDEN.
C     !DASH
      save
C     !DASH
      real*8 BOT, FL, H1, HE1, HE2, HE3, HK, HND, TE, TEN, TLL, TLR,
     $       TOP, XL, XNE, XR, XX
      integer KODE, LU, M, NH, NV
      logical GOOD
      character NUMERO*1, PERIOD*1
C     !COM
C---- IMAGE       as of 1997 Aug 21
      integer     IMALEN
      parameter   (IMALEN=65535)
      character   IMAGE*(IMALEN)
      common      /IMAGE/ IMAGE
C     Character string to hold plot images constructed by the
C     K-type line printer plotting routines;
C     but used also as a general scratch character array.
C     .
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
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
C     !DASH
      external COSMOS, KINIT, KRIGIA, MONKEY, ABJECT, LINER, KPRINT,
     $         HALT, PATH, HI, BYE
C
C               HND(N), H1(N), HK(N), XNE(N), HE1(N), HE2(N), HE3(N),
      dimension HND(*), H1(*), HK(*), XNE(*), HE1(*), HE2(*), HE3(*),
C
C               TE(N), XX(*), FL(N)
     $          TE(*), XX(*), FL(*)
C
      data TLL,TLR,BOT,TOP /3.8D0, 5.D0, 4.D0, 12.D0/
      data NV,NH /54, 117/
C     !EJECT
C
      call HI ('FENCE')
C     !BEG
      if((KODE.lt.1).or.(KODE.gt.2)) then
        write (MSSLIN(1),100) KODE
  100   format('KODE =',I12,', which is not 1 or 2.')
        call HALT   ('FENCE', 1)
      end if
C
C---- Establish X-coordinate
      call COSMOS   (KODE, M, TE, XX, XL, XR)
C---- Initialize plot image
      call KINIT    (IMAGE, XL, XR, BOT, TOP, NV, NH, NUMERO, GOOD)
      if(.not.GOOD) then
        call KRIGIA (XL, XR, BOT, TOP, NV, NH)
      end if
      if(KODE.eq.2) then
        call MONKEY (IMAGE, XL, XR, TEN, BOT, TOP, PERIOD, 1)
      end if
C---- Enter data tables into plot image
      call PATH     (IMAGE, M, XX, HE3, FL, NUMBS( 4))
      call PATH     (IMAGE, M, XX, HE2, FL, NUMBS( 3))
      call PATH     (IMAGE, M, XX, HE1, FL, NUMBS( 2))
      call PATH     (IMAGE, M, XX, XNE, FL, ALPHS( 5))
      call PATH     (IMAGE, M, XX, HK , FL, ALPHS(16))
      call PATH     (IMAGE, M, XX, H1 , FL, ALPHS(14))
      call PATH     (IMAGE, M, XX, HND, FL, ALPHS( 8))
C
C---- Print header, image, and legend
      call ABJECT   (LU)
      if(KODE.eq.1) then
        write (LU,101) M
  101   format(' ','Graph of log of H and He number densities vs. ',
     $             'log(TE), for depths #',I5,' through 1.')
      else
        write (LU,102)
  102   format(' ','Graph of log of H and He number densities vs. ',
     $             'depth index.')
      end if
      call KPRINT   (IMAGE, LU)
      call LINER    (1, LU)
      write (LU,103)
  103 format(' ','H = total H;  N = H(level 1);  P = protons; ',
     $           'E = electrons;  1 = He-I;  2 = He-II;  3 = He-III')
C     !END
      call BYE ('FENCE')
C
      return
      end
