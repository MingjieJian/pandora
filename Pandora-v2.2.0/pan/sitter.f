      subroutine SITTER
     $(J,KZANX,MN1,I)
C
C     Rudolf Loeser,2004 Jan 30
C---- Given J, an index of the augmented Z-table, finds I,
C     the corresponding index of the regular Z-table.
C     !DASH
      save
C     !DASH
      integer I, J, K, KZANX, MN1
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
C               KZANX(N)
      dimension KZANX(*)
C
      call HI ('SITTER')
C     !BEG
      I = 0
      do 100 K = 1,(MN1-1)
        if((J.ge.KZANX(K)).and.(J.lt.KZANX(K+1))) then
          I = K
          goto 102
        end if
  100 continue
      if(J.eq.KZANX(MN1)) then
        I = MN1
        goto 102
      end if
C
      write (MSSLIN(1),101) J
  101 format(' ','J =',I12,'; cannot place this expanded index.')
      call HALT ('SITTER', 1)
C
  102 continue
C     !END
      call BYE ('SITTER')
C
      return
      end
