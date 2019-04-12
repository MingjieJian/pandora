      subroutine JATOR
     $(I,IB,IE,IRR,M)
C
C     Rudolf Loeser, 1990 Nov 27
C---- Selects array members to be printed, for TOAD.
C     (Integer version of SATOR.)
C     !DASH
      save
C     !DASH
      integer I, IB, IE, IRR, M
C     !DASH
      external ZEROI, MOVEI, HI, BYE
C
C               I(*), IRR(8)
      dimension I(*), IRR(*)
C
      call HI ('JATOR')
C     !BEG
      call ZEROI (IRR,1,8)
      M = (IE-IB)+1
      call MOVEI (I(IB),1,M,IRR(2),1,M)
      M = M+1
C     !END
      call BYE ('JATOR')
C
      return
      end
