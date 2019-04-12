      subroutine IZZARD
     $(WAVCO,NCP,MULT,BANDL,BANDU,NAB,X,KODE)
C
C     Rudolf Loeser, 1983 Nov 14
C---- Determines which Composite Line Opacity wavelength bands are
C     affected by Opacity Multipliers .ne. 1.
C     !DASH
      save
C     !DASH
      real*8 BANDL, BANDU, FUDGE, ONE, WAVCO, X
      integer I, KODE, MULT, NAB, NB, NCP
      logical MEMBER
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external ZEROI, ZEMRUDE, KURASH, HI, BYE
C
      dimension X(*)
C
C               MULT(NAB), WAVCO(NCP), BANDL(NAB), BANDU(NAB)
      dimension MULT(*),   WAVCO(*),   BANDL(*),   BANDU(*)
C
      call HI ('IZZARD')
C     !BEG
      call ZEROI      (MULT,1,NAB)
      KODE = 0
C
      do 100 I = 1,NCP
        call ZEMRUDE  (X,WAVCO(I),FUDGE)
C
        if(FUDGE.ne.ONE) then
          call KURASH (NAB,BANDL,BANDU,WAVCO(I),MEMBER,NB)
          if(MEMBER) then
            MULT(NB) = 1
            KODE = 1
          end if
        end if
C
  100 continue
C     !END
      call BYE ('IZZARD')
C
      return
      end
