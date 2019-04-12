      subroutine SLUG
     $(NO,ISB1,ISB2)
C
C     Rudolf Loeser, 2003 May 09
C---- Prints information about an escape probability solution.
C     !DASH
      save
C     !DASH
      integer ISB1, ISB2, KVSB, NO
C     !COM
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(37),KVSB )
C     !DASH
      external HI, BYE
C
      call HI ('SLUG')
C     !BEG
      if(NO.gt.0) then
        write (NO,100) ISB1,ISB2
  100   format(' ','The Sobolev formula was used with ISB1 =',I3,' and',
     $             ' ISB2 =',I3,'; the necessary velocity values were')
        if(KVSB.eq.1) then
          write (NO,101)
  101     format(' ','obtained directly from input.')
        else if(KVSB.eq.2) then
          write (NO,102)
  102     format(' ','the Expansion velocity.')
        else if(KVSB.eq.3) then
          write (NO,103)
  103     format(' ','the first set of ',
     $               '"additional expansion velocity".')
        else
          write (NO,104) KVSB
  104     format(' ','weird: KVSB =',I10)
        end if
      end if
C     !END
      call BYE ('SLUG')
C
      return
      end
