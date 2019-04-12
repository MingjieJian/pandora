      subroutine FLAX
     $(NO,IU,IL,BTIT,RTIT)
C
C     Rudolf Loeser, 1980 Oct 30
C---- Prints a transition heading, for CROCUS.
C     !DASH
      save
C     !DASH
      integer IL, IU, NO
      character BTIT*4, RTIT*4, TLAB*33
C     !DASH
      external LINER, PADMA, HI, BYE
C
C               BTIT(4), RTIT(4)
      dimension BTIT(4), RTIT(4)
C
      call HI ('FLAX')
C     !BEG
      write (TLAB,100) IU,IL
  100 format('Transition [',I2,'/',I2,'], with analysis')
      call PADMA (NO, TLAB)
C
      write (NO,101) BTIT,RTIT
  101 format(' ','---------  RBD_/RBD*  --------',24X,
     $           '---------  RHO_/RHO*  --------',14X,
     $           '---- (for reference only) ---'/
     $       ' ',34X,'RBD* =',48X,'RHO* ='/
     $       ' ',2X,3(5X,A4),6X,A4,8X,'weight',3X,3(5X,A4),6X,A4,
     $           7X,'CHI',8X,'S',8X,'JBAR')
C
      call LINER (1, NO)
C     !END
      call BYE ('FLAX')
C
      return
      end
