      subroutine PECCARY
     $(IU,IL,KIJ,USE)
C
C     Rudolf Loeser, 1992 Mar 17
C---- Decides whether data for this transition should be printed.
C     (This is versin 2 of PECCARY.)
C     !DASH
      save
C     !DASH
      integer IL, IU, IUL, K, KIJ
      logical USE
C     !DASH
      external INDXUL, HI, BYE
C
C               KIJ(MUL)
      dimension KIJ(*)
C
      call HI ('PECCARY')
C     !BEG
      call INDXUL (IU,IL,IUL)
      K = KIJ(IUL)
C
      USE = (K.eq.1).or.(K.eq.2).or.(K.eq.3).or.(K.eq.4)
C     !END
      call BYE ('PECCARY')
C
      return
      end
