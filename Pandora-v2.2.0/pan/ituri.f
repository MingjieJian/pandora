      subroutine ITURI
     $(LU,EP1N,EP2N,N,T,LAB)
C
C     Rudolf Loeser, 1992 Dec 24
C---- Prints all Lyman EP1s and EP2s.
C     (This is version 2 of ITURI.)
C     !DASH
      save
C     !DASH
      real*8 E1, E2, EP1N, EP2N, T
      integer I, J, LU, N
      character L1*2, L2*2, LAB*9
C     !DASH
      external LINER, MOVED, KIPPER, SHIM, HI, BYE
C
C               EP1N(N,4), EP2N(N,4), T(4)
      dimension EP1N(N,*), EP2N(N,*), T(*)
C
      dimension E1(4), E2(4), LAB(4), L1(4), L2(4)
C
      call HI ('ITURI')
C     !BEG
      call LINER    (1,LU)
      write (LU,100) 'EP1','EP2',LAB,LAB
  100 format(' ',3X,2(4X,'-------------------------',1X,A3,2X,
     $                   '-------------------------',2X)/
     $       ' ',3X,2(2X,A12,3X,A12,3X,A12,3X,A12,3X))
C
      do 102 I = 1,N
        call MOVED  (EP1N(I,1),N,4,E1,1,4)
        call MOVED  (EP2N(I,1),N,4,E2,1,4)
        call KIPPER (E1,L1,4)
        call KIPPER (E2,L2,4)
C
        call SHIM   (I-1,5,LU)
        write (LU,101) I,(E1(J),L1(J),J=1,4),(E2(J),L2(J),J=1,4)
  101   format(' ',I3,1P,2(2X,E13.5,A2,E13.5,A2,E13.5,A2,E13.5,A2))
  102 continue
C
      call LINER    (2,LU)
      write (LU,103) LAB(1),T(1),LAB(2),T(2),LAB(3),T(3),LAB(4),T(4)
  103 format(' ','Times (sec) ',4(5X,A9,F9.3))
C     !END
      call BYE ('ITURI')
C
      return
      end
