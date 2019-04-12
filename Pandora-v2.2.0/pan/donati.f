      subroutine DONATI
     $(W,LU,LFB,NW,L,WTAB,LTYPE,EMINT,EMINTA,BRIGHT,BRIGHTA,LININT)
C
C     Rudolf Loeser, 1991 Jul 12
C---- Drives plotting, for BUDDHA.
C     !DASH
      save
C     !DASH
      real*8 BRIGHT, BRIGHTA, EMINT, EMINTA, W, WTAB, XL, XR
      integer IN, IS, IWLOG, KODE, L, LFB, LTYPE, LU, MOX, NW
      logical LININT
      character LABEL*15
C     !COM
C---- MOSTAR      as of 2000 Sep 26
      real*8      COREWL,COREWN
      integer     ICORE
      character   WLAB1*10,WLAB2*2,WLAB3*12,WLAB4*10,WLAB5*2
      logical     WAVENO,SLINE,WHOLE,RED,BLUE
      common      /MOSTAR1/ COREWL,COREWN
      common      /MOSTAR2/ ICORE
      common      /MOSTAR3/ WLAB1,WLAB2,WLAB3,WLAB4,WLAB5
      common      /MOSTAR4/ WAVENO,SLINE,WHOLE,RED,BLUE
C     Wavelength/Wavenumber print/plot controls (subroutine WUMBLE).
C     .
C     !DASH
C     !EJECT
      external LADDER, DANCE, RAMA, SITA, ANTIDO, WGIVE, HI, BYE
C
      dimension W(*)
C
C               EMINT(Nmkuse,L), BRIGHT(Nmkuse,L), BRIGHTA(Nmkuse),
      dimension EMINT(*),        BRIGHT(*),        BRIGHTA(*),
C
C               WTAB(Nmkuse), EMINTA(Nmkuse), LTYPE(Nmkuse)
     $          WTAB(*),      EMINTA(*),      LTYPE(*)
C
      dimension IN(1)
      equivalence
     $(IN( 1),IWLOG )
C
      call HI ('DONATI')
C     !BEG
      if((NW.gt.2).and.(LU.gt.0)) then
C       (Get, and allocate, W allotment)
        call ANTIDO (IN, IS, MOX, 'DONATI')
C
        LABEL = 'log(W'//WLAB1(2:)//')'
C----   Make logarithmic abscissa table (set =0 values to be omitted)
        call LADDER (WTAB, LTYPE, NW, W(IWLOG), LININT)
C----   Determine abscissa limits
        call DANCE  (W(IWLOG), NW, XL, XR, KODE)
        if(KODE.gt.0) then
C----     Plot continuous intensity
          call RAMA (LU, L, NW, W(IWLOG), XL, XR, LABEL, EMINT,  1,
     $               EMINTA,  LFB)
C----     Plot brightness temperature
          call SITA (LU, L, NW, W(IWLOG), XL, XR, LABEL, BRIGHT, 1,
     $               BRIGHTA, LFB)
        end if
C
C       (Give back W allotment)
        call WGIVE  (W, 'DONATI')
      end if
C     !END
      call BYE ('DONATI')
C
      return
      end
