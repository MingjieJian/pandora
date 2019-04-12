      subroutine DELVEN
     $(NCP,NAB,BANDL,BANDU,XLM,IBND,DOIT)
C
C     Rudolf Loeser, 1996 Feb 27
C---- Determines whether to do scattering albedo analysis
C     at this wavelength.
C     !DASH
      save
C     !DASH
      real*8 BANDL, BANDU, XLM
      integer IBND, NAB, NCP
      logical DOIT
C     !DASH
      external KURASH, HI, BYE
C
C               BANDL(NAB), BANDU(NAB)
      dimension BANDL(*),   BANDU(*)
C
      call HI ('DELVEN')
C     !BEG
      DOIT = NCP.gt.0
      if(DOIT) then
        call KURASH (NAB,BANDL,BANDU,XLM,DOIT,IBND)
      end if
C     !END
      call BYE ('DELVEN')
C
      return
      end
