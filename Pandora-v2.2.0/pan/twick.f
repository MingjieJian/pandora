      subroutine TWICK
     $(KILROY,CALLER,MRHO,IU,IL,I,BDJIT)
C
C     Rudolf Loeser, 1983 Nov 01
C---- Prints error message for VITALE.
C     !DASH
      save
C     !DASH
      real*8 BDJIT
      integer I, IL, IU, LUEO, MRHO
      logical KILROY
      character CALLER*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MESHED, HI, BYE
C
      call HI ('TWICK')
C     !BEG
      if(KILROY) then
        KILROY = .false.
        call MESHED (CALLER, 3)
      end if
      write (LUEO,100) IU,IL,MRHO,I,BDJIT
  100 format(' ','Error calculating S* for transition (',I2,'/',I2,')'/
     $       ' ','MRHO =',I2,', depth # =',I4,', BD =',1PE20.12)
C     !END
      call BYE ('TWICK')
C
      return
      end
