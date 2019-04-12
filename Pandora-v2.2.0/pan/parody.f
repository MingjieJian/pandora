      subroutine PARODY
     $(X,W,IW,GMASIN,G,Z,N,XCBL,IMG,RECZ,MODE)
C
C     Rudolf Loeser, 2003 Nov 03
C---- Computes Z from input mass and the function G computed in the
C     HSE calculation.
C     MODE = 1 means: HSE
C     MODE = 2 means: initialization
C     !DASH
      save
C     !DASH
      real*8 G, GMASIN, W, X, XCBL, Z
      integer IGG, IISWA, IMG, IN, IOP5, IP, IS, ITAU5, IW, IWS, IY,
     $        IZOLD, IZZ, JN, KMASN, MODE, MOX, MUX, N
      logical RECZ
C     !COM
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(30),KMASN)
C     !DASH
C     !EJECT
      external DROYPA, DRAYPO, PRODAY, WGIVE, IGIVE, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               GMASIN(N), XCBL(Miklen), IMG(N), G(N), Z(N)
      dimension GMASIN(*), XCBL(*),      IMG(*), G(*), Z(*)
C
      dimension IN(7)
      equivalence
     $(IN( 1),IZOLD ),(IN( 2),IP    ),(IN( 3),IY    ),(IN( 4),IGG   ),
     $(IN( 5),IZZ   ),(IN( 6),IOP5  ),(IN( 7),ITAU5 )
C
      dimension JN(1)
      equivalence
     $(JN( 1),IISWA )
C
      call HI ('PARODY')
C     !BEG
      RECZ = KMASN.gt.0
      if(RECZ) then
C       (Get, and allocate, W & IW allotments)
        call DROYPA (IN, IS , MOX, 'PARODY')
        call DRAYPO (JN, IWS, MUX, 'PARODY')
C
        call PRODAY (GMASIN, G, Z, N, X, W, IW, IMG, XCBL, W(IZOLD),
     $               W(IP), W(IY), W(IGG), W(IZZ), W(IOP5), W(ITAU5),
     $               IW(IISWA), MODE)
C
C       (Give back W & IW allotments)
        call WGIVE  (W , 'PARODY')
        call IGIVE  (IW, 'PARODY')
      end if
C     !END
      call BYE ('PARODY')
C
      return
      end
