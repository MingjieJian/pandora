      subroutine OLBIA
     $(N,NMTS,Z,ARR,ZL,ARRL,KODE,XNP,ZHEL,XNE,R,RL,LIM,NO,LABEL)
C
C     Rudolf Loeser, 1982 Feb 16
C---- Produces graphs, for POCKET.
C     !DASH
      save
C     !DASH
      real*8 ARR, ARRL, GRID, R, RL, XL, XNE, XNP, XR, Z, ZHEL, ZL
      integer I, IBEG, IEND, KNT, KODE, LGRD, LIM, N, NG, NH, NMTS, NO,
     $        NV
      character LABEL*(*), TIT*10
C     !COM
C---- ELEMENT     as of 1998 Aug 17
      integer     NELX
      parameter   (NELX=50)
C     (Remember to recompile all users when changing NELX)
      real*8      ELABD,ELCHI,ELLU1,ELLU2,ELABL,ELDEF
      integer     LATNO,LDEFR,NMT,NMTMAX
      logical     LATEM
      character   ELSYM*3, ELSUB*3
      dimension   ELSYM(NELX),ELSUB(NELX),ELABD(NELX),ELCHI(NELX),
     $            ELLU1(NELX),ELLU2(NELX),ELABL(NELX),ELDEF(NELX),
     $            LATNO(NELX),LDEFR(NELX),LATEM(NELX)
C
      common      /ELEMNT0/ NMT,NMTMAX
      common      /ELEMNT1/ ELSYM,ELSUB
      common      /ELEMNT2/ ELABD,ELCHI,ELLU1,ELLU2,ELABL,ELDEF
      common      /ELEMNT3/ LATNO,LDEFR
      common      /ELEMNT4/ LATEM
C
C     Element data tables:
C             ELSYM - element symbol;
C             ELSUB - (Scratch storage for I.D. symbols);
C             ELABD - abundance (w.r.t. Hydrogen);
C             ELCHI - Chi, i.e. ionization potential;
C             ELLU1 - U-I partition function;
C             ELLU2 - U-II partition function;
C             ELABL - logarithmic abundance;
C             ELDEF - defaults values of logarithmic abundance;
C             LATNO - atomic number; and
C             LDEFR - default values sources codes.
C             LATEM - "metal" designator
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
C     !EJECT
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
C     !DASH
      external METALIC, ABJECT, LINER, GASIC, BIALO, HI, BYE
C
C               Z(N), ARR(N,NMT), XNE(N), ARRL(N,NMT), ZHEL(N), XNP(N),
      dimension Z(*), ARR(*),     XNE(*), ARRL(*),     ZHEL(*), XNP(*),
C
C               ZL(N), R(N), RL(N)
     $          ZL(*), R(*), RL(*)
C
      parameter (NG=7)
      dimension GRID(NG), LGRD(NG)
C
      data GRID  /1.D0, 5.D-1, 2.D-1, 1.D-1, 5.D-2, 2.D-2, 1.D-2/
      data NV,NH /52, 117/
C
      call HI ('OLBIA')
C     !BEG
      call METALIC  (N,NMTS,Z,ARR,ZL,ARRL,IMAGE,TIT,GRID,NG,NV,NH,KNT,
     $               IBEG,IEND,XL,XR)
      if(KODE.eq.1) then
        call GASIC  (N,IMAGE,KNT,ZL,IBEG,IEND,XNE,XNP,ZHEL,R,RL)
      end if
C
      if(KNT.gt.LIM) then
        call ABJECT (NO)
        write (NO,100) LABEL,TIT
  100   format(' ','Graph of log(',A,') vs. ',A10)
C
        call LINER  (1,NO)
        call BIALO  (IMAGE,NO,GRID,NG,LGRD,XL,XR,NV,TIT)
        call LINER  (1,NO)
C
        if(KODE.eq.1) then
          write (NO,101)
  101     format(' ','+: protons',10X,'*: electrons from Helium')
        end if
C
        write (NO,102) (ALPHS(I),ELSUB(I),I=1,NMTS)
  102   format(12(' ',A1,': ',A3,:,3X))
      end if
C     !END
      call BYE ('OLBIA')
C
      return
      end
