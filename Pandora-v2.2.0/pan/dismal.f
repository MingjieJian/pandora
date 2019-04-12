      subroutine DISMAL
     $(X,W,IBEG,IEND,OPAC,TAU,LABEL,KODE,EDINT,EDTAU,IMG)
C
C     Rudolf Loeser, 1980 Sep 22
C---- Uses a set of opacity values, OPAC (/cm), to compute a
C     set of optical depth values, TAU.
C     Upon return, KODE .eq. 0 if the set of TAU values is
C     non-decreasing, KODE .gt. 0 if not.
C     (This is version 4 of DISMAL.)
C     !DASH
      save
C     !DASH
      real*8 OPAC, TAU, W, X
      integer IBEG, IEND, IFINT, IMG, IN, IS, JJPRF, JJTKI, JJZ, KODE,
     $        KTKIN, MOX
      logical EDINT, EDTAU
      character LABEL*(*)
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ(126),JJPRF)
      equivalence (IZOQ( 75),JJTKI)
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
      equivalence (LEST(11),KTKIN)
C     !DASH
C     !EJECT
      external LAUD, DANK, DREAR, WGIVE, HI, BYE
C
      dimension X(*), W(*)
C
C               OPAC(N), IMG(N), TAU(N)
      dimension OPAC(*), IMG(*), TAU(*)
C
      dimension IN(1)
      equivalence
     $(IN( 1),IFINT)
C
      call HI ('DISMAL')
C     !BEG
C     (Get W allotment)
      call LAUD    (IN, IS, MOX, 'DISMAL')
C
      if(KTKIN.eq.0) then
C       This is straight integration over Z
        call DANK  (IBEG, IEND, OPAC, X(JJZ),             W(IFINT),
     $              TAU, LABEL, KODE, EDINT, EDTAU, IMG, W)
      else
C       This is integration over TAUK, which requires that the
C       integrand be divided by PREF (= X(JJPRF))
        call DREAR (IBEG, IEND, OPAC, X(JJPRF), X(JJTKI), W(IFINT),
     $              TAU, LABEL, KODE, EDINT, EDTAU, IMG, W)
      end if
C
C     (Give back W allotment)
      call WGIVE   (W, 'DISMAL')
C     !END
      call BYE ('DISMAL')
C
      return
      end
