      subroutine TEUFEL
     $(X,W,XCBL,N,TNU,XLB,SP,TAUK,KK,XK,IADRS,XNUK,XNU,NPOPS,OPAC,IMG,
     $ XKKA,XKKB,GK,KOLEV,XJIKA,TRK)
C
C     Rudolf Loeser, 1979 Nov 30
C---- Gets and manipulates Continuum Data for the Lyman calculations.
C     (This is version 2 of TEUFEL.)
C     !DASH
      save
C     !DASH
      real*8 DNU, FNU, GK, OPAC, SP, TAUK, TNU, TRK, W, X, XCBL, XJIKA,
     $       XK, XKKA, XKKB, XLB, XLM, XNU, XNUK
      integer IADRS, IMG, IQLYD, ISLV, ITYPE, K, KK, KKBHSR, KKCAPR,
     $        KKISLV, KKJNU, KKMULT, KKSIGM, KKT1, KKTR, KOLEV, N,
     $        NPOPS
      logical DUMP
C     !COM
C---- COBLOCK     as of 2005 Mar 04
      integer     NKKK,MIKLEN,KKK
      parameter   (NKKK=59)
C     (Remember to recompile GERIN when changing NKKK)
      dimension   KKK(NKKK)
      common      /COBLOCK/ MIKLEN, KKK
C     Continuum Data Block components index.
      equivalence (KKK( 2),KKMULT)
      equivalence (KKK(22),KKT1  )
      equivalence (KKK(23),KKTR  )
      equivalence (KKK(28),KKCAPR)
      equivalence (KKK(30),KKSIGM)
      equivalence (KKK(27),KKBHSR)
      equivalence (KKK(13),KKJNU )
      equivalence (KKK(33),KKISLV)
C
C---- OPTIONS     as of 2007 Jan 12
C
C     Processing and printing control switches.
C
      integer     NOOPT
      parameter   (NOOPT=345)
C     (When NOOPT is changed, FOP, FURRY, REFAULT must be recompiled!)
      integer     IQQ,IQD,IQT
      character   ONAME*8
      dimension   IQQ(NOOPT),IQD(NOOPT),IQT(NOOPT), ONAME(NOOPT)
C
      common      /OPTIONS/ IQQ
C     IQQ is the actual option status.
      common      /OPTION1/ IQD
C     IQD is the default option status.
      common      /OPTION2/ ONAME
C     ONAME is the option name (use 0000 for unused names).
      common      /OPTION3/ IQT
C     IQT is the option type:
C     1 = printout; 2 = calculation; 3 = miscellaneous; 4 = debug.
      equivalence (IQQ(116),IQLYD)
C     !DASH
C     !EJECT
      external FUTILE, ANGIE, DEVIL, LETTER, TYKE, MOVE1, HEXE, MASHED,
     $         BRIT, HI, BYE
C
      dimension X(*), W(*)
C
C               XJIKA(N,KKX), TRK(N,KKX), GK(KKX), XKKA(N,KKX), IMG(N),
      dimension XJIKA(N,*),   TRK(N,*),   GK(*),   XKKA(N,*),   IMG(*),
C
C               XLB(N,KKX), SP(N,KKX), XK(KKX), XCBL(Miklen), XNU(NSL),
     $          XLB(N,*),   SP(N,*),   XK(*),   XCBL(*),      XNU(*),
C
C               IADRS(KKX), OPAC(N,KKX), XKKB(N,KKX), TNU(N,KKX),
     $          IADRS(*),   OPAC(N,*),   XKKB(N,*),   TNU(N,*),
C
C               TAUK(N)
     $          TAUK(*)
C
      data ITYPE /0/
C     !EJECT
C
      call HI ('TEUFEL')
C     !BEG
      call FUTILE    (N, KK, DUMP, 'TEUFEL')
      DNU = XNUK-XNU(KOLEV)
C
C---- Loop over all frequencies
      do 100 K = 1,KK
C
C----   Compute Continuum Block Name
        FNU = DNU*XK(K)
        call ANGIE   (FNU, XLM)
C----   Read Continuum Block
        call LETTER  (IADRS(K), XCBL, XLM, ITYPE, 'TEUFEL')
C----   Get Continuum Data
        ISLV = XCBL(KKISLV)
        call DEVIL   (N, K, XCBL(KKMULT), XCBL(KKT1), XCBL(KKTR),
     $                XCBL(KKCAPR), XCBL(KKSIGM), XCBL(KKBHSR),
     $                XCBL(KKJNU), XKKA(1,K), XKKB(1,K), XLB(1,K),
     $                SP(1,K), OPAC(1,K), XLM, IADRS(K), DUMP)
C----   Save XJNU
        call MOVE1   (XCBL(KKJNU), N, XJIKA(1,K))
C----   Compute radiation temperature
        call BRIT    (FNU, N, XCBL(KKJNU), TRK(1,K))
C----   Compute optical depth
        call TYKE    (X, W, KOLEV, K, N, OPAC(1,K), TNU(1,K), IMG)
        if(K.eq.1) then
C----     Save optical depth at head of continuum
          call MOVE1 (TNU(1,K), N, TAUK)
        end if
C
        if(DUMP) then
          call HEXE  (K, N, XK(K), IADRS(K), XLM, XCBL(KKMULT), GK(K),
     $                ISLV, XKKA(1,K), XKKB(1,K), TNU(1,K), SP(1,K),
     $                XLB(1,K), XCBL(KKJNU), XCBL(KKSIGM),
     $                XCBL(KKCAPR), XCBL(KKT1), XCBL(KKTR),
     $                XCBL(KKBHSR))
        end if
C
  100 continue
      if(DUMP) then
        call MASHED  ('TEUFEL')
      end if
C     !END
      call BYE ('TEUFEL')
C
      return
      end
