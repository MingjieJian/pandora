      subroutine FISH
     $(NO,LINES,IS,IE,LINE,J,WTAB,INDX,KIPE)
C
C     Rudolf Loeser, 1974 Mar 29
C---- Prints a line for BENJAMIN.
C     !DASH
      save
C     !DASH
      real*8 WTAB
      integer I, IE, INDX, IS, J, JS, KIPE, LINES, NO
      character DASH*6, LEG*17, LINE*105
C     !COM
C---- OPACITY     as of 2007 Jan 12
C     Paraphernalia for background absorption/emission contributors.
C
C     (Must recompile BARE, BRACE, FORAGER & SHARI when changing NABS!)
      parameter   (NABS=45)
C
      integer     NABS,NOPAC,KOPAC,TOPAC,LOPAC
      character   CNAME*24,SYMID*1,SHNAM*6
      dimension   KOPAC(NABS),LOPAC(NABS),SYMID(NABS)
      dimension   CNAME(NABS),SHNAM(NABS)
C
      common      /OPAC1/ NOPAC
      common      /OPAC2/ KOPAC
      common      /OPAC3/ LOPAC
      common      /OPAC4/ CNAME
      common      /OPAC5/ SYMID
      common      /OPAC6/ SHNAM
C
C     NOPAC = number of contributors
C     KOPAC = contributor status switch: 0 = don't use, 1 = use
C     CNAME = name (description) of contributor
C             NOTE: This sequence of names establishes the indices or
C                   ordinals by which the contributors are also known.
C     SHNAM = "short" name of contributor
C     LOPAC = "printout order" list of contributor ordinals
C     SYMID = scratch space for symbolic identifiers
C     .
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
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
      external ABJECT, HALT, LINER, SHIM, HI, BYE
C
C               WTAB(Numkon), INDX(Numkon)
      dimension WTAB(*),      INDX(*)
C
      dimension LEG(3)
C
      data DASH /'   ---'/
      data LEG /'A B S O R B E R S', 'E M I T T E R S', 'T A U S'/
C     !EJECT
C
      call HI ('FISH')
C     !BEG
      if(NO.gt.0) then
C
        if(LINES.ge.40) then
C----     Print page heading, if needed
          call ABJECT (NO)
          LINES = 0
          JS    = IS+1
C
          if((KIPE.lt.1).or.(KIPE.gt.3)) then
            write (MSSLIN(1),100) KIPE
  100       format('KIPE =',I12,', which is not 1, 2, or 3.')
            call HALT ('FISH', 1)
          end if
C
          write (NO,101) LEG(KIPE)
  101     format(' ',19X,A17)
          call LINER  (1, NO)
C
          write (NO,102) WLAB1(2:),(DASH,I=IS,IE,2)
  102     format(' ',2X,'W',A9,4X,17A6)
          write (NO,103) WLAB4,(I,I=IS,IE,2)
  103     format(' ',2X,A10,4X,17I6)
          write (NO,104) (SYMID(I),I=IS,IE,2)
  104     format(' ',16X,17(5X,A1))
          write (NO,105) (SHNAM(I),I=IS,IE,2)
  105     format(' ',16X,17A6)
          call LINER  (1, NO)
C
          write (NO,106) (DASH,I=JS,IE,2)
  106     format(' ',19X,17A6)
          write (NO,107) (I,I=JS,IE,2)
  107     format(' ',19X,17I6)
          write (NO,108) (SYMID(I),I=JS,IE,2)
  108     format(' ',19X,17(5X,A1))
          write (NO,109) (SHNAM(I),I=JS,IE,2)
  109     format(' ',19X,17A6)
          call LINER  (1, NO)
        end if
C
C----   Now the regular line
        write (NO,110) WTAB(J),INDX(J),LINE
  110   format(' ',1PE12.4,I4,3X,A105)
        LINES = LINES+1
        call SHIM     (J, 5, NO)
      end if
C     !END
      call BYE ('FISH')
C
      return
      end
