      subroutine HIDRE
     $(MRR,IQSFS)
C
C     Rudolf Loeser, 1981 Oct 27
C---- Checks MRR for spherical source function calculations.
C     (This is version 3 of HIDRE.)
C     !DASH
      save
C     !DASH
      integer IQSFS, MRR
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external HALT, HI, BYE
C
      call HI ('HIDRE')
C     !BEG
      if(IQSFS.gt.0) then
        if(MRR.le.1) then
          write (MSSLIN(1),100) MRR
  100     format('MRR =',I12,5X,'too few Disk Rays for spherical ',
     $           'calculation.')
          call HALT ('HIDRE',1)
        end if
      end if
C     !END
      call BYE ('HIDRE')
C
      return
      end
