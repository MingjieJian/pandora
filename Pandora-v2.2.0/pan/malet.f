      subroutine MALET
     $(I,NBOP,MIK,ISR,KODE)
C
C     Rudolf Loeser, 1982 Sep 22
C---- Dumps, for VULCAN.
C     !DASH
      save
C     !DASH
      integer I, ISR, KODE, LUEO, MIK, NBOP
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, HI, BYE
C
      call HI ('MALET')
C     !BEG
      call LINER (1, LUEO)
      write (LUEO,100) I,NBOP,MIK,ISR,KODE
  100 format(' ','Frequency  XI(',I3,')',5X,'NBOP',I6,5X,'MIK',I2,5X,
     $           'ISR',I2,5X,'KODE',I2)
C     !END
      call BYE ('MALET')
C
      return
      end
