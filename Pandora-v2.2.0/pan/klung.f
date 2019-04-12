      subroutine KLUNG
     $(NO,NW,FR,WTAB,FHZ,FMM,LFB,LINFLX)
C
C     Rudolf Loeser, 1979 Oct 31
C---- Makes Flux plots, for APEX.
C     !DASH
      save
C     !DASH
      real*8 FHZ, FMM, FR, WTAB, ZERO
      integer LFB, NO, NW
      logical LINFLX
      character FACELAB*10, LINE*122, TIT*23
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
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
      external TUMBLE, ABJECT, ACME, HI, BYE
C
C               FR(Nmkuse), WTAB(Nmkuse), FHZ(Nmkuse), FMM(Nmkuse)
      dimension FR(*),      WTAB(*),      FHZ(*),      FMM(*)
C
      data TIT /'Log(Flux at earth) vs. '/
C
      call HI ('KLUNG')
C     !BEG
      if((NW.gt.2).and.(NO.gt.0)) then
        call TUMBLE (LFB,FACELAB)
C
C----   Flux vs. frequency
        call ABJECT (NO)
        LINE = 'Log(Flux at star) vs. log(Frequency)'//'     '//FACELAB
        write (NO,100) LINE
  100   format(' ',A)
        call ACME   (NO, NW, FR,   FHZ, IMAGE, LINFLX)
C
C----   Flux vs. wavelength/wavenumber
        call ABJECT (NO)
        LINE = TIT//'log(W'//WLAB1(2:)//')'//'     '//FACELAB
        write (NO,100) LINE
        call ACME   (NO, NW, WTAB, FMM, IMAGE, LINFLX)
      end if
C     !END
      call BYE ('KLUNG')
C
      return
      end
