      subroutine POOLE
     $(I,KODE,MODE,YES)
C
C     Rudolf Loeser, 2003 Aug 19
C---- Tells whether the I'th continuum wavelength belongs to the
C     subset specified by KODE.
C     (This is version 7 of POOLE.)
C     !DASH
      save
C     !DASH
      integer I, KODE, MODE
      logical YES
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external FOAM, ROAM, LOAM, HALT, HI, BYE
C
      call HI ('POOLE')
C     !BEG
      if((KODE.lt.1).or.(KODE.gt.3)) then
        write (MSSLIN(1),100) KODE
  100   format('KODE =',I12,', which is neither 1, 2, or 3.')
        call HALT ('POOLE', 1)
      end if
C
      goto (101, 102, 103), KODE
  101 continue
C----   "General"
        call FOAM (I, YES)
        goto 199
  102 continue
C----   LYMAN
        call ROAM (I, YES)
        goto 199
  103 continue
C----   Current transition
        call LOAM (I, MODE, YES)
        goto 199
C
  199 continue
C     !END
      call BYE ('POOLE')
C
      return
      end
