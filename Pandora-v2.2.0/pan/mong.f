      subroutine MONG
     $(NO,N,ZME,RZM,AEL,XNP,XNE,ZHEL,ETA,FMC,FTC,ZMEL,ZMER,KODE)
C
C     Rudolf Loeser, 1969 Jul 28
C     Revised RL/SGK Apr 15 2014 
C---- Prints metal electrons data.
C     !DASH
      save
C     !DASH
      real*8 AEL, ETA, FMC, FTC, RZM, XNE, XNP, ZHEL, ZME, ZMEL, ZMER
      integer I, IE, IS, KODE, N, NO
      character BLANK*1, TIT*9
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
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
C     !EJECT
      external  PRIAM, LINER, LOCKUP, HI, BYE
      intrinsic min
C
C               RZM(N), AEL(N), XNP(N), ETA(N,NMT), FMC(N,NMT), XNE(N),
      dimension RZM(*), AEL(*), XNP(*), ETA(*),     FMC(*),     XNE(*),
C
C               ZME(N), FTC(N), ZMEL(N), ZHEL(N), ZMER(N)
     $          ZME(*), FTC(*), ZMEL(*), ZHEL(*), ZMER(*)
C
      call HI ('MONG')
C     !BEG
      call PRIAM    (NO,'ELECTRONS',9)
      call LINER    (2,NO)
      write (NO,100)
  100 format(' ','Electron Contributors other than Hydrogen or Helium')
      if(KODE.eq.0) then
        TIT = BLANK
      else
        TIT = 'Weighted '
      end if
C
      IE = 0
  101 continue
        IS = IE+1
        IE = min(IE+10,N)
        call LINER  (2,NO)
        write (NO,102) (I,I=IS,IE)
  102   format(' ',21X,'Depth ',10I10)
        call LOCKUP (FMC,IS,IE,N,NO,'Relative Contribution')
        call LINER  (1,NO)
        write (NO,103) (FTC(I),I=IS,IE)
  103   format(' ',21X,'Total ',10F10.6)
        call LOCKUP (ETA,IS,IE,N,NO,' Degree of Ionization')
        call LINER  (1,NO)
        write (NO,104) (ZMER(I),I=IS,IE)
  104   format(' ',10X,    'Z = "Metals"/HND ',1P10E10.2)
        write (NO,105) (RZM(I),I=IS,IE)
  105   format(' ',11X,    'Multiplier of Z ',1P10E10.2)
        write (NO,106) (AEL(I),I=IS,IE)
  106   format(' ',13X,    'Addition to Z ',1P10E10.2)
        call LINER  (1,NO)
        write (NO,107) (ZMEL(I),I=IS,IE)
  107   format(' ',10X,    'Metals ion dens. ',1P10E10.2)
        write (NO,108) (ZHEL(I),I=IS,IE)
  108   format(' ',10X,    'Helium ion dens. ',1P10E10.2)
        write (NO,109) (XNP(I),I=IS,IE)
  109   format(' ',14X,    'Proton dens. ',1P10E10.2)
        write (NO,110) TIT,(XNE(I),I=IS,IE)
  110   format(' ',3X,A9,  'Electron dens. ',1P10E10.2)
      if(IE.lt.N) goto 101
C     !END
      call BYE ('MONG')
C
      return
      end
