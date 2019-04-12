      subroutine THUNDER
     $(IS,IE,WTAB,KODE,NO,P1,P2)
C
C     Rudolf Loeser, 2000 Oct 23
C---- Prints a header, for HAIL.
C     KODE = 1 for continuum; =2 for line.
C
C     (This is version 2 of THUNDER.)
C     !DASH
      save
C     !DASH
      real*8 WTAB
      integer IE, IS, KODE, NO
      character P1*8, P2*8
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
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external LINER, HALT, IBOR, NERO, ORNI, HI, BYE
C
C               WTAB(KM or Nmkuse), P1(14), P2(14)
      dimension WTAB(*),            P1(*),  P2(*)
C     !EJECT
C
      call HI ('THUNDER')
C     !BEG
      call LINER      (1,NO)
C
      if(WAVENO) then
        call IBOR     (IS,IE,WTAB,NO,P1,P2)
      else
        if(KODE.eq.1) then
          call NERO   (IS,IE,WTAB,NO,P1,P2)
        else if (KODE.eq.2) then
          call ORNI   (IS,IE,WTAB,NO,P1)
        else
          write (MSSLIN(1),100) KODE
  100     format('KODE =',I12,', which is neither 1 nor 2.')
          call HALT   ('THUNDER',1)
        end if
      end if
C
      call LINER      (1,NO)
C     !END
      call BYE ('THUNDER')
C
      return
      end
