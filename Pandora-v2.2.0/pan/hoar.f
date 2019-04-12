      subroutine HOAR
     $(NO,SHORT,LEGEND,KODE,KOELS)
C
C     Rudolf Loeser, 1980 Dec 08
C---- Prints heading, for OOBLECK.
C     !DASH
      save
C     !DASH
      integer KODE, KOELS, NO
      logical LEGEND, SHORT
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
      external DASHER, LINER, DOBRAWA, HI, BYE
C
      call HI ('HOAR')
C     !BEG
      call LINER    (2,NO)
      if(LEGEND) then
        call DASHER (NO)
        LEGEND = .false.
        write (NO,100) WLAB1
  100   format(' ','Explanation of  DEPTHS-OF-FORMATION  analyses:'//
     $         ' ','At each ',A,', the Intensity is computed as ',
     $             'the sum of contributions from each depth point.'//
     $         ' ','In this printout, these contributions are printed ',
     $             'as percentages (rounded to the nearest integer) ',
     $             'of their sum.')
        call LINER  (2,NO)
C     !EJECT
        if(SHORT) then
          write (NO,101) WLAB1
  101     format(' ','The first column on the left lists depth ',
     $               'indices.'/
     $           ' ','The first row gives ',A,' indices, as shown on ',
     $               'the preceding Intensity listing.'//
     $           ' ','(A more detailed printout can be obtained by ',
     $               'turning the option ORSHORT off.)')
        else
          write (NO,102)
  102     format(' ','The signal  <<  identifies the largest ',
     $               'contribution. The two  *  signals indicate the ',
     $               'two endpoints of that set of'/
     $           ' ','contributions, including the maximum, whose sum ',
     $               'constitutes 0.9 of the total sum. ',
     $               'The two  -  signals indicate'/
     $           ' ','in the same way the set whose sum constitutes ',
     $               '0.6 of the total sum.'//
     $           ' ','(A compressed version of this printout can be ',
     $               'obtained by turning the option ORSHORT on.)')
        end if
        call LINER     (1,NO)
        if(KOELS.eq.0) then
          write (NO,103)
  103     format(' ','To print every depth, even if the line is ',
     $               'blank, set KOELS = 1.')
        else
          write (NO,104)
  104     format(' ','To omit printing lines at depths with all ',
     $               'blanks, set KOELS = 0.')
        end if
        if(KODE.eq.1) then
          call LINER   (1,NO)
          call DOBRAWA (NO,WAVENO)
        end if
        call LINER     (1,NO)
        write (NO,105)
  105   format(' ','(This same explanation holds for all ',
     $             'DEPTHS-OF-FORMATION analyses printed later, ',
     $             'but will not be printed again.)')
        call LINER     (1,NO)
        call DASHER    (NO)
      else
        write (NO,106)
  106   format(' ','(An explanation of this DEPTHS-OF-FORMATION ',
     $             'analysis was printed earlier.)')
      end if
C     !END
      call BYE ('HOAR')
C
      return
      end
