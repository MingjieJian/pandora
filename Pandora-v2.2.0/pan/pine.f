      subroutine PINE
     $(TAU,N,WN,REF,FIN,TMS,W)
C
C     Rudolf Loeser, 1971 Jul 09
C---- Computes WN, RT Weight Matrix for Source Function calculations.
C     !DASH
      save
C     !DASH
      real*8 TAU, TMS, W, WN
      integer IDT, IN, IS, ISE2, ISE3, ISE4, ISES2, ISES3, ISES4, IT,
     $        MOX, N
      logical FIN, REF
C     !DASH
      external NEEDLE, SPRUCE, WGIVE, HI, BYE
C
      dimension W(*)
C
C               TAU(N), WN(N,N)
      dimension TAU(*), WN(*)
C
      dimension IN(8)
      equivalence
     $(IN( 1),IT    ),(IN (2),IDT   ),(IN( 3),ISE2  ),(IN( 4),ISES2 ),
     $(IN( 5),ISE3  ),(IN( 6),ISES3 ),(IN( 7),ISE4  ),(IN( 8),ISES4 )
C
      call HI ('PINE')
C     !BEG
C     (Get, and allocate, W allotment)
      call NEEDLE (IN,IS,MOX,'PINE',N)
C
      call SPRUCE (TAU,N,WN,REF,FIN,TMS,W(IT),W(IDT),W(ISE2),W(ISE3),
     $             W(ISE4),W(ISES2),W(ISES3),W(ISES4))
C
C     (Give back W allotment)
      call WGIVE  (W,'PINE')
C     !END
      call BYE ('PINE')
C
      return
      end
