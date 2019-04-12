      subroutine ALARUM
     $(KUP,LLO,IU,IL,LIJ,DOSNGLE)
C
C     Rudolf Loeser, 2003 Nov 17
C---- Decides whether the single-rate formula should be used for the
C     "Term added to upward C" in the VAMOS method.
C
C     KUP,LLO are the transition indices of the term being computed
C     IU,IL   are the indices of the transition for which the
C              statistical equilibrium equations are being computed.
C     LIJ     chooses the formula:
C              =1 for net-rate formula [Chi] (default)
C              =2 for single-rate formula [J-bar]
C
C---- Upon return, DOSNGLE = .true. means: use single-rate formula.
C
C     See also QUINOA.
C     !DASH
      save
C     !DASH
      integer IL, IU, IUL, KUP, LIJ, LLO
      logical DOSNGLE, TREQUAL, USINGLE
C     !DASH
      external INDXUL, HI, BYE
C
C               LIJ(MUL)
      dimension LIJ(*)
C
      call HI ('ALARUM')
C     !BEG
      call INDXUL (KUP, LLO, IUL)
C
      USINGLE = LIJ(IUL).eq.2
      TREQUAL = (IU.eq.KUP) .and. (IL.eq.LLO)
C
      DOSNGLE = USINGLE .and. (.not.TREQUAL)
C     !END
      call BYE ('ALARUM')
C
      return
      end
