      subroutine LEYTE
     $(RECORD,LENGTH,IADRS)
C
C     Rudolf Loeser, 1986 Jul 08
C---- "Reads" a scratch I/O record.
C     (See detailed remarks in "VISAYAS".)
C     (This is version 2 of LEYTE.)
C     !DASH
      save
C     !DASH
      real*8 RECORD
      integer IADRS, IOSTAT, LENGTH, LUEO, LUNRA
      logical GOOD
C     !COM
C---- MACTAN      as of 1998 Apr 03
      real*8      FILDAT
      dimension   FILDAT(11)
      common      /MACTAN/ FILDAT
C---- Control parameters for the PANDORA random-access scratch file.
C     .
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MERTRN, RAREAD, IOFAULT, MASBATE, PANGLAO, MESHED,
     $         ABORT, HI, BYE
C
C               RECORD(LENGTH)
      dimension RECORD(*)
C
      call HI ('LEYTE')
C     !BEG
      if(IADRS.lt.0) then
        call MERTRN    (RECORD, LENGTH, (-IADRS))
      else
        call PANGLAO   (LENGTH, 'LEYTE')
        call RAREAD    (FILDAT, RECORD, (8*LENGTH), IADRS, GOOD, IOSTAT)
        if(.not.GOOD) then
          call MESHED  ('LEYTE', 1)
          LUNRA = FILDAT(1)
          call IOFAULT (LUEO, 'LEYTE', 'Read', LUNRA, IOSTAT)
          call MASBATE (LUEO)
          call ABORT
        end if
      end if
C     !END
      call BYE ('LEYTE')
C
      return
      end
