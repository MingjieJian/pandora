      subroutine TAURUS
     $(X,W,IW,KKU,XKX,AKX,GKX,EP1,EP2,Z,TNU,OPAC,EMUX,V,XLB,D,F1,XLP,
     $ RNDT,SP,XLF,TK,S,ERT,IMG,WNSAV,IXUSE,IETA,KASE)
C
C     Rudolf Loeser, 1974 Nov 20
C---- Calculates "LYMAN" source function.
C     (This is version 3 of TAURUS.)
C     !DASH
      save
C     !DASH
      real*8 AKX, D, EMUX, EP1, EP2, ERT, F1, GKX, OPAC, RNDT, S, SP,
     $       TK, TNU, V, W, WNSAV, X, XKX, XLB, XLF, XLP, XNUK, YL, Z
      integer IETA, IMG, IN, IP, IS, ISIG, ISO, IW, IXUSE, KASE, KKU,
     $        MOX, N, NE
      logical DMPW, DUMP
      character LABEL*100
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (RZQ(  9),XNUK )
      equivalence (RZQ( 19),YL   )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
C     !EJECT
      external NORA, ILONA, SAPELE, LAUGH, PALDAO, WGIVE, STUMP, SENTA,
     $         HALT, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               ERT(N), XKX(KKX), AKX(KKX), EP2(N), IMG(N), IXUSE(KKX),
      dimension ERT(*), XKX(*),   AKX(*),   EP2(*), IMG(*), IXUSE(*),
C
C               F1(N), D(N), TNU(N,KKX), OPAC(N,KKX), XLP(N), V(N,KKX),
     $          F1(*), D(*), TNU(*),     OPAC(*),     XLP(*), V(*),
C
C               WNSAV(N,N,KKX), XLB(N,KKX), RNDT(N), SP(N,KKX), XLF(N),
     $          WNSAV(*),       XLB(*),     RNDT(*), SP(*),     XLF(*),
C
C               EP1(N), TK(N), GKX(KKX), S(N), Z(N), EMUX(N,KKX)
     $          EP1(*), TK(*), GKX(*),   S(*), Z(*), EMUX(*)
C
      dimension IN(2)
      equivalence
     $(IN( 1),IP    ),(IN( 2),ISO   )
C
      data LABEL /'Lyman Source Function'/
C     !EJECT
C
      call HI ('TAURUS')
C     !BEG
C     (Get W allotment)
      call NORA     (IN, IS, MOX, 'TAURUS')
C
C---- Get dump output control parameters
C     DUMP is for Lyman dump
C     DMPW is for WN-matrix dump
      call ILONA    (DUMP, DMPW)
C
      NE = IETA
      if((KASE.eq.1).or.(KASE.eq.3)) then
C----   Do frequency summations
        call SAPELE (X, W, IW, TNU, OPAC, Z, EMUX, XKX, KKU, AKX, YL,
     $               GKX, V, XLB, D, F1, W(IP), IETA, N, IMG, WNSAV,
     $               IXUSE, DUMP, DMPW)
        call LAUGH  (AKX, GKX, XKX, KKU, XNUK, XLB, XLP, SP, V, WNSAV,
     $               IXUSE, IETA, N, XLF)
C----   Compute Source Function
        call PALDAO (EP1, IETA, W(IP), S, EP2, F1, RNDT, XLF, W, IW,
     $               ISIG, DMPW)
        if(ISIG.le.0) then
          NE = 0
        end if
C
      else if(KASE.ne.2) then
        write (MSSLIN(1),100) KASE
  100   format('KASE =',I12,', which is not 1, 2, or 3.')
        call HALT   ('TAURUS', 1)
      end if
C
C---- Set remaining values of S, if necessary.
      call STUMP    (NE, N, S, ERT)
C---- Edit S
      call SENTA    (S, TK, N, LABEL, W(ISO), DUMP)
C
C     (Give back W allotment)
      call WGIVE    (W, 'TAURUS')
C     !END
      call BYE ('TAURUS')
C
      return
      end
