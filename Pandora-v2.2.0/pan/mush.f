      subroutine MUSH
C
C     Rudolf Loeser, 1981 Aug 13
C---- Deals with the output Heading.
C     (This is version 3 of MUSH.)
C     !DASH
      save
C     !DASH
      integer NO
C     !COM
C---- HEADER      as of 1984 Apr 23
      character   HEAD*80
      common      /HEADER/ HEAD
C     Copy of the "HEADER" line for this run.
C     .
C---- VERSION     as of 2006 May 16
      real*8      VERSION
      integer     NVDSCR
      character   VDSCRPT*63
      dimension   VDSCRPT(45)
      common      /VERSION1/ VERSION
      common      /VERSION2/ NVDSCR
      common      /VERSION3/ VDSCRPT
C     Identifier and description of this version of PANDORA.
C     (Values set by subroutine AARDVRK.)
C     .
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
C     !DASH
C     !EJECT
      external HUSH, ABJECT, LINER, LUSH, RUSH, HI, BYE
C
      call HI ('MUSH')
C     !BEG
C---- Make banner pages (if needed)
      call HUSH
C
C
C
C---- Write sign-on page
      call ABJECT (NO)
      call LINER  (10, NO)
      write (NO,100)
  100 format(' ',52X,'***  P A N D O R A  ***'//
     $       ' ',53X,'General Printout File'//
     $       ' ',62X,'for')
C
      call LINER  (8, NO)
      write (NO,101) HEAD
  101 format(' ',31X,A)
      call LINER  (8, NO)
C
      write (NO,102) VERSION
  102 format(' ',52X,'Program Version',F8.3)
C
C
C
C---- Initialize printout index file (if needed)
      call LUSH
C---- Initialize error mesaage file (if needed)
      call RUSH
C     !END
      call BYE ('MUSH')
C
      return
      end
