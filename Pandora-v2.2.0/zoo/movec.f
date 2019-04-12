      subroutine MOVEC
     $(A,INCA,NA,B,INCB,NB)
C     Rudolf Loeser, 1979 Apr 05
C---- Given the array A of length NA, and the array B of length NB.
C     A and B are both type CHARACTER.
C     This routine copies the contents of A into B as follows:
C                  A(1+0*INCA) into B(1+0*INCB),
C                  A(1+1*INCA) into B(1+1*INCB),
C                  A(1+2*INCA) into B(1+2*INCB),
C                              etc.
C
C---- If NA.gt.NB, then only the first NB elements of A will be copied
C     into B. If NB.gt.NA, then the last (NB-NA) elements of B will NOT
C     be changed by this routine.
C     !DASH
      save
C     !DASH
      integer I, INA, INB, INCA, INCB, M, NA, NB
      character A*(*), B*(*)
C     !DASH
      intrinsic min
C
      dimension A(*), B(*)
C
C     !BEG
      M = min(NA,NB)
      if(M.gt.0) then
        INA = 1-INCA
        INB = 1-INCB
        do 100 I = 1,M
          INA = INA+INCA
          INB = INB+INCB
          B(INB) = A(INA)
  100   continue
      end if
C     !END
C
      return
      end
