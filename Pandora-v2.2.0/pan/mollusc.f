      subroutine MOLLUSC
     $(WONE,W,N,INDXA,INDXB)
C
C     Rudolf Loeser, 1999 Jan 07
C---- Establishes a table of "weights."
C     Returns WONE = .true. if all = 1.
C     (This is version 2 of MOLLUSC.)
C     !DASH
      save
C     !DASH
      real*8 D, DI, W
      integer I, ID, INDXA, INDXB, N
      logical WONE
C     !DASH
      external ONE1, ZERO1, HI, BYE
C
C               W(N)
      dimension W(*)
C
      call HI ('MOLLUSC')
C     !BEG
      call ONE1    (W, N)
      WONE = .true.
C
      if(INDXA.lt.N) then
        call ZERO1 (W(INDXB), (N-INDXB+1))
        ID = INDXB-INDXA
C
        if(ID.gt.1) then
          DI = ID
          do 100 I = (INDXA+1),(INDXB-1)
            D    = INDXB-I
            W(I) = D/DI
  100     continue
        end if
C
      WONE = .false.
      end if
C     !END
      call BYE ('MOLLUSC')
C
      return
      end
