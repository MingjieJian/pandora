      subroutine ZEBUS
     $(NO,WTAB,K)
C
C     Rudolf Loeser, 2001 Oct 16
C---- Auxiliary printout for BUZES.
C     !DASH
      save
C     !DASH
      real*8 WTAB
      integer I, IE, IS, K, LL, NO
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
      external  LINER, HI, BYE
      intrinsic min
C
C               WTAB(K)
      dimension WTAB(*)
C
      call HI ('ZEBUS')
C     !BEG
      if(NO.gt.0) then
C
        call LINER     (2,NO)
        write (NO,100) WLAB3
  100   format(' ',A)
C
        LL = 5
        IE = 0
  101   continue
          IS = IE+1
          IE = min((IE+5),K)
          if(LL.eq.5) then
            call LINER (1,NO)
            LL = 0
          end if
          write (NO,102) IS,IE,(WTAB(I),I=IS,IE)
  102     format(' ',I3,'-',I3,1P5E24.15)
          LL = LL+1
        if(IE.lt.K) goto 101
C
      end if
C     !END
      call BYE ('ZEBUS')
C
      return
      end
