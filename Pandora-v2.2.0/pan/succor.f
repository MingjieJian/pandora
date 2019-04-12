      subroutine SUCCOR
     $(NO,N,IU,IL,BDIJ,RHO,WEIGHT,YBAR,S,CHI)
C
C     Rudolf Loeser, 2004 Mar 17
C---- Prints without analysis, for CROCUS.
C     !DASH
      save
C     !DASH
      real*8 BDIJ, CHI, RBD, RHO, S, WEIGHT, YBAR
      integer I, IL, IU, N, NO
      character LAB*47
C     !DASH
      external PADMA, LINER, BRAT, SHIM, HI, BYE
C
C               BDIJ(N,NL), RHO(N), WEIGHT(N), YBAR(N), CHI(N), S(N)
      dimension BDIJ(*),    RHO(*), WEIGHT(*), YBAR(*), CHI(*), S(*)
C
      call HI ('SUCCOR')
C     !BEG
      write (LAB,100) IU,IL
  100 format('Transition [',I2,'/',I2,'], analysis suppressed (JBDNC)')
      call PADMA  (NO, LAB)
C
      write (NO,101)
  101 format(' ',16X,'RBD',6X,'weight',13X,'RHO',13X,'CHI',15X,'S',
     $           12X,'JBAR')
      call LINER  (1,NO)
C
      do 103 I = 1,N
        call BRAT (I, IU, IL, BDIJ, RBD)
        write (NO,102) I,RBD,WEIGHT(I),RHO(I),CHI(I),S(I),YBAR(I)
  102   format(' ',I5,1PE16.8,E12.1,4E16.8)
        call SHIM (I, 5, NO)
  103 continue
C     !END
      call BYE ('SUCCOR')
C
      return
      end
