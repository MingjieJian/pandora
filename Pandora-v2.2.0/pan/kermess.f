      subroutine KERMESS
     $(LU,KMSS)
C
C     Rudolf Loeser, 2002 Nov 21
C---- Sets the editing/smoothing/weighting message switch.
C     !DASH
      save
C     !DASH
      integer KMSS, MO
      character LU*2
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C     !DASH
      external HI, BYE
C
      call HI ('KERMESS')
C     !BEG
      KMSS = 1
      if(LU.eq.'MO') then
        if(MO.le.0) then
          KMSS = 0
        end if
      end if
C     !END
      call BYE ('KERMESS')
C
      return
      end
