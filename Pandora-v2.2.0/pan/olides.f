      subroutine OLIDES
     $(NO,KODE,WTAB,EMU,YY,MUX,MYX,DMAX,PLOT,XLTIT,RUN,N,OK)
C
C     Rudolf Loeser, 1991 Aug 09
C---- Prints, for OSTUNI.
C     !DASH
      save
C     !DASH
      real*8 DMAX, EMU, RUN, WTAB, XLTIT, YY
      integer I, KODE, MUX, MYX, N, NF, NO
      logical OK
      character BLANK*1, BRIEF*10, FK*13, HEADER*40, MARK*3, PLOT*1,
     $          W*17
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
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
C     !DASH
C     !EJECT
      external MONGOL, LINER, ENCODED, TISRIT, HI, BYE
C
C               RUN(N)
      dimension RUN(*)
C
      call HI ('OLIDES')
C     !BEG
      call LINER    (1, NO)
C
      call ENCODED  (WTAB, W(6:17), 12, 9, 1, NF)
      W(:5) = WLAB2//' = '
C
      call TISRIT   (MUX, YY, MYX, FK)
C
      if(PLOT.ne.BLANK) then
        MARK = '['//PLOT//']'
      else
        MARK = BLANK
      end if
C
      if(KODE.eq.2) then
        call MONGOL (XLTIT, HEADER, BRIEF)
        write (NO,100) W,EMU,FK,DMAX,MARK,HEADER
  100   format(' ',A,'  mu =',F6.3,1X,A,1X,'max(dI/dh) =',1PE12.4,1X,
     $             A,2X,A)
      else
        write (NO,100) W,EMU,FK,DMAX,MARK
      end if
C
      call LINER    (1, NO)
      if(OK) then
        write (NO,101) (RUN(I),I=1,N)
  101   format(' ',15F8.3)
      else
        write (NO,102)
  102   format(' ','***** Error: max(dI/dh) = zero; dI/dh was not ',
     $             'normalized and just the raw values are printed.')
        write (NO,103) (RUN(I),I=1,N)
  103   format(' ',1P10E12.4)
      end if
C     !END
      call BYE ('OLIDES')
C
      return
      end
