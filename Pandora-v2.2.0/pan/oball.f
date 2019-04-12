      subroutine OBALL
     $(NO,KODE,IU,IL,ICDIT,DETAIL,Z,TE,N)
C
C     Rudolf Loeser, 1991 AUG 09
C---- Prints a header, for OSTUNI.
C     !DASH
      save
C     !DASH
      real*8 TE, Z
      integer ICDIT, IL, IU, KODE, N, NO
      logical DETAIL
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
      external ABJECT, LINER, VECOUT, HI, BYE
C
C               TE(N), Z(N)
      dimension TE(*), Z(*)
C
      call HI ('OBALL')
C     !BEG
      call ABJECT (NO)
C
      if(KODE.eq.1) then
        if(DETAIL) then
          write (NO,100) IU,IL
  100     format(' ','dI/dh for all points of the intensity profile ',
     $               'of the (',I2,'/',I2,') line',43X,'Option DIDHL'/
     $           ' ',6X,'(To see line cores only, set IDFSW = 0.)')
        else
          write (NO,101) IU,IL
  101     format(' ','dI/dh for line cores of the intensity profile ',
     $               'of the (',I2,'/',I2,') line',43X,'Option DIDHL'/
     $           ' ',6X,'(To see all points of the profile, ',
     $               'set IDFSW = 1.)')
        end if
      else
        write (NO,102) ICDIT
  102   format(' ','dI/dh for selected continuum intensity values',
     $             52X,'ICDIT =',I2,', Option DIDHC')
      end if
C
      if(DETAIL) then
        call LINER (1, NO)
        write (NO,103) WLAB1
  103   format(' ','(The [letter] in brackets identifies ',A,' values',
     $           ' that are included in the plot which follows.)')
      end if
C
      call VECOUT  (NO, Z,  N, 'Z' )
      call VECOUT  (NO, TE, N, 'TE')
C     !END
      call BYE ('OBALL')
C
      return
      end
