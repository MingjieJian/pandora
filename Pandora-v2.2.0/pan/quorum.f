      subroutine QUORUM
     $(KUP,LLO,IU,IL,IX,SINGLE)
C
C     Rudolf Loeser, 2003 Nov 17
C---- Decides whether the single-rate formula should be used for the
C     "Term added to upward C" in the VAMOS method.
C
C     KUP,LLO are the transition indices of the term being computed
C     IU,IL   are the indices of the transition for which the
C              statistical equilibrium equations are being computed.
C
C---- Upon return, SINGLE = .true. means: use single-rate formula.
C     !DASH
      save
C     !DASH
      integer IL, IU, IX, JJLIJ, KUP, LLO
      logical SINGLE
C     !COM
C---- MINIGER     as of 2006 Jan 12
      integer     JBSCR,JZOQ
      dimension   JZOQ(17)
      common      /MINIGER/ JBSCR,JZOQ
C     INTEGER*4 General Data Block components index.
      equivalence (JZOQ(  8),JJLIJ)
C     !DASH
      external ALARUM, HI, BYE
C
      dimension IX(*)
C
      call HI ('QUORUM')
C     !BEG
      call ALARUM (KUP, LLO, IU, IL, IX(JJLIJ), SINGLE)
C     !END
      call BYE ('QUORUM')
C
      return
      end
