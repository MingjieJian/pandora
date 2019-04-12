      subroutine LILTO
     $(IIFLAG,ITS,CALLER)
C
C     Rudolf Loeser, 1978 Apr 09
C---- Finishes the dump for CSF calculation.
C     !DASH
      save
C     !DASH
      integer IIFLAG, ITS, LUEO
      character CALLER*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MASHED, LINER, HI, BYE
C
      call HI ('LILTO')
C     !BEG
      call LINER  (1, LUEO)
      write (LUEO,100) IIFLAG,ITS
  100 format(' ','IIFLAG',I10,10X,'ITS',I4)
      call MASHED (CALLER)
C     !END
      call BYE ('LILTO')
C
      return
      end
