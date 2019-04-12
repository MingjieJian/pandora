      subroutine THALIA
     $(X,W,N,RHEAB,Z,HND,ZI,Z2,Z3,ZT,TE,HK,H1,HE1,BETA,HE2K,HEND,DEE,
     $ ZION,KVLG,VBMB,VM,DUMP)
C
C     Rudolf Loeser, 1991 Jan 03
C---- Recompute RHEAB (and VM), if needed.
C     !DASH
      save
C     !DASH
      real*8 ASTAR, BETA, CFHE, CHEFL, CVM, DEE, FACT, H1, HE1, HE2K,
     $       HEABD, HEABL, HEMASS, HEND, HK, HND, RFHEA, RHEAB, TE,
     $       VBMB, VM, W, X, Z, Z2, Z3, ZERO, ZI, ZION, ZT, dummy
      integer ICAY, ICUE, IDRH, IEFF, IELL, IFR, IGEE, IHEAB, IHEDF, IN,
     $        IQHEA, IQVLG, IRHO, IS, IWHY, IYR, IZR, J, JHEAB, JHEAS,
     $        K, KODE, KVLG, LU, MOX, N
      logical DOIT, DUMP, EDIT, STOP
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 47),IHEDF)
      equivalence (RZQ(100),CHEFL)
      equivalence (RZQ(152),CFHE )
      equivalence (RZQ(115),HEABL)
      equivalence (KZQ( 83),IHEAB)
      equivalence (RZQ(116),RFHEA)
      equivalence (KZQ( 72),JHEAS)
C
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(59),JHEAB)
C
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (PCON( 5),HEMASS)
C     !EJECT
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
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
      equivalence (IQQ(281),IQHEA)
      equivalence (IQQ(221),IQVLG)
C     !DASH
      external NOSONI, HURSAG, NABUSSI, GALAGO, HURRUM, KOKOPU, ALATHI,
     $         HULCA, MOVE1, KLUTI, FRANK, KAPUTT, WGIVE, LATHIA, SAGA,
     $         HI, BYE
C
      dimension X(*), W(*)
C
C               RHEAB(N), DEE(4,5,N), HEND(N), HND(N), BETA(N), HE1(N),
      dimension RHEAB(*), DEE(4,5,*), HEND(*), HND(*), BETA(*), HE1(*),
C
C               HK(N), Z2(N), Z3(N), ZT(N), ZI(N), H1(N), TE(N), VM(N),
     $          HK(*), Z2(*), Z3(*), ZT(*), ZI(*), H1(*), TE(*), VM(*),
C
C               Z(N), HE2K(N), ZION(N), VBMB(N)
     $          Z(*), HE2K(*), ZION(*), VBMB(*)
C
      dimension IN(11)
      equivalence
     $(IN( 1),IRHO  ),(IN( 2),IELL  ),(IN( 3),IWHY  ),(IN( 4),ICAY  ),
     $(IN( 5),ICUE  ),(IN( 6),IEFF  ),(IN( 7),IGEE  ),(IN( 8),IDRH  ),
     $(IN( 9),IZR   ),(IN(10),IFR   ),(IN(11),IYR   )
C
      data FACT /1.D-15/
      data STOP /.true./
C     !EJECT
C
      call HI ('THALIA')
C     !BEG
      call KAPUTT      (IQHEA, IHEDF, JHEAS, DUMP, DOIT)
C
      if(DOIT) then
C       (Get, and allocate, W allotment)
        call NOSONI    (IN, IS, MOX, 'THALIA')
C
C----   Get Helium abundance and mass
        call FRANK     ('HE ', 0, HEABD, dummy, dummy, dummy, KODE)
        if(KODE.eq.0) then
          call SAGA    ('HE ', 'THALIA', STOP)
        end if
C----   Save old RHEAB (for printing [ ? ])
        call MOVE1     (RHEAB, N, W(IRHO))
C----   Compute L
        call KOKOPU    (N, CHEFL, DEE, HND, W(IELL))
C----   Compute y, K, Q, and a*
        CVM = CFHE*FACT
        call HULCA     (N, IQVLG, HEABD, RHEAB, ASTAR, DEE, ZI, Z2,
     $                  Z3, ZT, W(ICUE), HEMASS, W(IELL), CVM, HND,
     $                  W(ICAY), Z, W(IWHY), W(IZR), W(IFR), W(IYR),
     $                  IHEAB)
C----   Compute F and G
        call GALAGO    (N, W(IWHY), W(IELL), W(IGEE), Z, HEABD,
     $                  W(IEFF), W(IZR), W(IFR), W(IYR), IHEAB)
C----   Compute new RHEAB
        call HURSAG    (N, W(IWHY), W(IEFF), RHEAB)
        JHEAB = 1
C----   Compute DRHEAB
        call HURRUM    (N, DEE, HK, H1, HE1, BETA, HE2K, TE, W(IDRH))
C
        if(DUMP) then
          call NABUSSI (N, HEABD, HEMASS, IHEAB, CHEFL, HEABL, RFHEA,
     $                  CVM, IQVLG, ASTAR, W(IELL), W(ICUE), W(ICAY),
     $                  W(IWHY), W(IEFF), RHEAB, W(IDRH), Z, W(IRHO),
     $                  DEE, HND, ZION, ZI, TE, ZT, Z2, Z3)
        end if
C----   Edit RHEAB
        call KLUTI     (N, HEABL, RHEAB, EDIT)
C----   Recompute  HEND and VM
        call LATHIA    (X, W, N, RHEAB, HEND, HND, KVLG, VBMB, VM)
        if(DUMP) then
C----     Print final values
          call ALATHI  (LU, N, HND, HEND, RHEAB, VBMB, VM)
        end if
C
C       (Give back W allotment)
        call WGIVE     (W, 'THALIA')
      else
C     !EJECT
C----   Set row #2 of d-matrix equal to zero
        do 101 K = 1,N
          do 100 J = 1,5
            DEE(2,J,K) = ZERO
  100     continue
  101   continue
      end if
C     !END
      call BYE ('THALIA')
C
      return
      end
