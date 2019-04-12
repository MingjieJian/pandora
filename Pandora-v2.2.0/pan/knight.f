      subroutine KNIGHT
     $(KODE,KABS,KEMIT)
C
C     Rudolf Loeser, 1997 Oct 20
C---- Checks control codes for OUSE; aborts the run if necessary.
C     (This is version 2 of KNIGHT.)
C     !DASH
      save
C     !DASH
      integer KABS, KEMIT, KODE, LUEO
      logical KILROY, OK
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MUSHED, ABORT, HI, BYE
C
      data KILROY /.true./
C
      call HI ('KNIGHT')
C     !BEG
      OK = .true.
C
      if((KODE.lt.1).or.(KODE.gt.3)) then
        call MUSHED ('KNIGHT', 1, KILROY)
        write (LUEO,100) KODE
  100   format(' ','KODE =',I12,'which is neither 1, 2, or 3.')
        OK = .false.
      end if
C
      if((KEMIT.eq.1).and.(KABS.eq.0)) then
        call MUSHED ('KNIGHT', 1, KILROY)
        write (LUEO,101) KEMIT,KABS
  101   format(' ','KEMIT =',I12,', KABS =',I12,'; this combination ',
     $             'does not make sense.')
        OK = .false.
      end if
C
      if(.not.OK) then
        call ABORT
      end if
C     !END
      call BYE ('KNIGHT')
C
      return
      end
