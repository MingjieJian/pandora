      subroutine RUSH
C
C     Rudolf Loeser, 2002 Jul 12
C---- Initializes error message file.
C     !DASH
      save
C     !DASH
      integer LUEO, NO
C     !COM
C---- HEADER      as of 1984 Apr 23
      character   HEAD*80
      common      /HEADER/ HEAD
C     Copy of the "HEADER" line for this run.
C     .
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external STARER, LINER, HI, BYE
C
      call HI ('RUSH')
C     !BEG
      if(LUEO.ne.NO) then
C----   Copy Heading to error file
        write (LUEO,100) HEAD
  100   format(' ','Error/advisory messages output file for:'//
     $         ' ',A)
        call LINER  (2, LUEO)
        call STARER (LUEO)
        call STARER (LUEO)
      end if
C     !END
      call BYE ('RUSH')
C
      return
      end
