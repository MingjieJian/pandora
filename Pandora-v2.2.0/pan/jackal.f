      subroutine JACKAL
     $(NL,JU,JL,MTR)
C
C     Rudolf Loeser, 1978 Sep 13
C---- Sets up transition indices, for GECKO.
C     (This is version 2 of JACKAL.)
C     !DASH
      save
C     !DASH
      integer IL, IU, IUL, JL, JU, MTR, NL
C     !DASH
      external  INDXUL, HI, BYE
C
C               JU(MUL), JL(MUL)
      dimension JU(*),   JL(*)
C
      call HI ('JACKAL')
C     !BEG
      MTR = 0
      do 101 IU = 2,NL
        do 100 IL = 1,(IU-1)
          MTR = MTR+1
          call INDXUL (IU,IL,IUL)
          JU(IUL) = IU
          JL(IUL) = IL
  100   continue
  101 continue
C     !END
      call BYE ('JACKAL')
C
      return
      end
