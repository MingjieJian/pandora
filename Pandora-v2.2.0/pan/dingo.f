      subroutine DINGO
     $(NO,N,PFT,Z,RAT,IPNT,PFE,ZLOG)
C
C     Rudolf Loeser, 1984 Aug 16
C---- Plots partition function ratios, for 20 selected ions.
C     (This is version 2 of DINGO.)
C     !DASH
      save
C     !DASH
      real*8 ONE, PFE, PFT, RAT, SIG, Z, ZLOG
      integer I, IBEG, IEND, IPNT, MSUB, N, NO, NSUB
      character ELED*3, TIT*10
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
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !EJECT
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
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
      external  DURHAM, LOGO, ZEBRA, SHRIMP, ABJECT, LINER, KPRINT,
     $          HI, BYE
      intrinsic min
C
      parameter (MSUB=20)
      dimension ELED(MSUB)
C
C               PFT(N,NMT), RAT(NMT), IPNT(NMT), PFE(N,MSUB), ZLOG(N),
      dimension PFT(*),     RAT(*),   IPNT(*),   PFE(*),      ZLOG(*),
C
C               Z(N)
     $          Z(*)
C
      call HI ('DINGO')
C     !BEG
      if(NO.gt.0) then
        NSUB = min(NMT,MSUB)
        SIG  = ZL10SMA
C----   Get edited table
        call DURHAM (NMT, N, PFT, ELSYM, NSUB, PFE, ELED, RAT, IPNT)
C----   Convert to logs
        call LOGO   (PFE, (N*NSUB), 1, SIG, PFE)
C----   Initialize plot image
        call ZEBRA  (Z, ZLOG, N, NSUB, IBEG, IEND, PFE, SIG, ONE,
     $               IMAGE, TIT, 'DINGO')
C----   Enter points into image
        call SHRIMP (ZLOG, N, IBEG, IEND, PFE, NSUB, ALPHS, 36, SIG,
     $               2, IMAGE)
C----   Write heading, graph, and legend
        call ABJECT (NO)
        write (NO,100) TIT
  100   format(' ','Graph of log10(Ratios of Partition Functions) ',
     $             'vs. ',A10)
        call LINER  (1, NO)
        call KPRINT (IMAGE, NO)
C
        call LINER  (1, NO)
        write (NO,101) (ALPHS(I),ELED(I),I=1,NSUB)
  101   format(10(' ',A1,':',A3,4X))
      end if
C     !END
      call BYE ('DINGO')
C
      return
      end
