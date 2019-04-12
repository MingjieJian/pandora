      subroutine QUINOA
     $(KUP,JLO, IU,IL, KRJ,LIJ, DOSNGLE)
C
C     Rudolf Loeser, 1996 Mar 07
C---- Decides whether the single-rate formula should be used for the
C     "Term added to upward C".
C                                 > Not for VAMOS!
C
C     KUP,JLO are the transition indices of the term being computed
C     IU,IL   are the indices of the transition for which the
C              statistical equilibrium equations are being computed;
C              they should be (0,0) otherwise.
C     KRJ     tells about input:
C              =1 for RHO with JBAR
C              =2 for only JBAR
C              =3 for only RHO
C     LIJ     chooses the formula when KRJ=1:
C              =1 for net-rate formula [rho] (default)
C              =2 for single-rate formula [J-bar]
C
C---- Upon return, DOSNGLE = .true. means: use single-rate formula.
C
C     See also QUORUM.
C     !DASH
      save
C     !DASH
      integer IL, IU, JLO, KRJ, KUP, LIJ
      logical DOSNGLE, INJB, INRJ, SING1, SING2, TREQUAL, USNG
C     !DASH
      external HI, BYE
C
      call HI ('QUINOA')
C     !BEG
      USNG    = LIJ.eq.2
      INRJ    = KRJ.eq.1
      INJB    = KRJ.eq.2
      TREQUAL = (IU.eq.KUP) .and. (IL.eq.JLO)
C
      SING1   = INJB
      SING2   = INRJ .and. USNG .and. (.not.TREQUAL)
C
      DOSNGLE = SING1 .or. SING2
C     !END
      call BYE ('QUINOA')
C
      return
      end
