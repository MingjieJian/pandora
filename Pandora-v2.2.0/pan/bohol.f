      subroutine BOHOL
     $(RECORD,LENGTH,IADRS)
C
C     Rudolf Loeser, 1986 Jul 08
C---- "Rewrites" a scratch I/O record.
C     (See detailed remarks in "VISAYAS".)
C     (This is version 5 of BOHOL.)
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
      external MEUPDA, RARERI, MASBATE, PANGLAO, MESHED, IOFAULT,
     $         ABORT, HI, BYE
C
C               RECORD(LENGTH)
      dimension RECORD(*)
C
      call HI ('BOHOL')
C     !BEG
      if(IADRS.lt.0) then
        call MEUPDA    (RECORD,LENGTH,-IADRS)
      else
        call PANGLAO   (LENGTH,'BOHOL')
        call RARERI    (FILDAT,RECORD,8*LENGTH,IADRS,GOOD,IOSTAT)
        if(.not.GOOD) then
          call MESHED  ('BOHOL',1)
          LUNRA = FILDAT(1)
          call IOFAULT (LUEO,'BOHOL','Rewrite',LUNRA,IOSTAT)
          call MASBATE (LUEO)
          call ABORT
        end if
      end if
C     !END
      call BYE ('BOHOL')
C
      return
      end
