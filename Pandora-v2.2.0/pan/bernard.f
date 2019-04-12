      subroutine BERNARD
     $(IU,IL,K,DDR,CDR,CRIT,ITER)
C
C     Rudolf Loeser, 1992 Apr 08
C---- Prints for TABOR.
C     !DASH
      save
C     !DASH
      real*8 CDR, CRIT, DDR
      integer I, IL, ITER, IU, K, LUEO
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
C               DDR(K), CDR(K)
      dimension DDR(*), CDR(*)
C
      call HI ('BERNARD')
C     !BEG
      call LINER (2, LUEO)
      write (LUEO,100) IU,IL,K,ITER,CRIT
  100 format(' ','Coalesced tables for transition (',I2,'/',I2,'), ',
     $           'K =',I6//
     $       ' ','iteration',I3,5X,'"resolvability" criterion',1PE16.8//
     $       ' ',9X,'i',21X,'DWN',21X,'CDL')
      call LINER (1, LUEO)
C
      write (LUEO,101) (I,DDR(I),CDR(I),I=1,K)
  101 format(5(' ',I10,1P2E24.16/))
C     !END
      call BYE ('BERNARD')
C
      return
      end
