      subroutine FRAGAR
     $(MULT,NAB,NID)
C
C     Rudolf Loeser, 1983 Nov 14
C---- Converts band-markers (possibly non-contiguous) into consecutive
C     band-indices.
C     !DASH
      save
C     !DASH
      integer I, MULT, NAB, NID
C     !DASH
      external HI, BYE
C
C               MULT(NAB)
      dimension MULT(*)
C
      call HI ('FRAGAR')
C     !BEG
      NID = 0
      do 100 I = 1,NAB
        if(MULT(I).gt.0) then
          NID = NID+1
          MULT(NID) = I
        end if
  100 continue
C     !END
      call BYE ('FRAGAR')
C
      return
      end
