      subroutine CEBU
     $(RECORD,LENGTH,IADRS)
C
C     Rudolf Loeser, 1986 Jul 08
C---- "Writes" a scratch I/O record.
C     (See detailed remarks in "VISAYAS".)
C     (This is version 2 of CEBU.)
C     !DASH
      save
C     !DASH
      real*8 RECORD
      integer IADRS, IOSTAT, KIMS, LENGTH, LUEO, LUMA, LUNRA, NW
      logical GOOD, KILROY
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
      equivalence (LUNITS(19),LUMA )
C
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(41),KIMS )
C     !DASH
      external PANGLAO, MESPAC, VISAYAS, RARITE, IOFAULT, MASBATE,
     $         MEACPT, MESHED, ABORT, HI, BYE
C
C               RECORD(Length)
      dimension RECORD(*)
C
      data KILROY /.true./
C     !EJECT
C
      call HI ('CEBU')
C     !BEG
      call MESPAC      (NW)
      if((NW.gt.LENGTH).and.(KIMS.eq.1)) then
C
        call MEACPT    (RECORD, LENGTH, IADRS)
        IADRS = -IADRS
C
      else
C
        if(KILROY) then
          KILROY = .false.
          KIMS  = 0
          call VISAYAS (LUMA)
        end if
C
        call PANGLAO   (LENGTH, 'CEBU')
        IADRS = 0
        call RARITE    (FILDAT, RECORD, (8*LENGTH), IADRS, GOOD, IOSTAT)
C
        if(.not.GOOD) then
          call MESHED  ('CEBU', 1)
          LUNRA = FILDAT(1)
          call IOFAULT (LUEO, 'CEBU', 'Write', LUNRA, IOSTAT)
          call MASBATE (LUEO)
          call ABORT
        end if
C
      end if
C     !END
      call BYE ('CEBU')
C
      return
      end
