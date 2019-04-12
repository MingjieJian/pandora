      subroutine ANDRIA
     $(KTIT,WVL,LABEL,NC)
C
C     Rudolf Loeser, 2000 Aug 25
C---- Makes a label, for RANI.
C     (This is version 4 of ANDRIA.)
C     !DASH
      save
C     !DASH
      real*8 WVL
      integer KTIT, NC
      character BLANK*1, LABEL*(*)
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external HALT, HI, BYE
C
      call HI ('ANDRIA')
C     !BEG
      if((KTIT.lt.1).or.(KTIT.gt.2)) then
        write (MSSLIN(1),100) KTIT
  100   format('KTIT =',I12,', which is not 1 or 2.')
        call HALT ('ANDRIA', 1)
      end if
C
      LABEL = BLANK
      if(KTIT.eq.1) then
        write (LABEL,101) WVL
  101   format('"Eclipse" intensity at',1PE20.12,' Angstroms; shell ',
     $         'ray tangent to radius #')
        NC = 83
      else
        write (LABEL,102) WVL
  102   format('Intensity at',1PE20.12,' Angstroms, for continuum ',
     $         'flux in spherical coordinates; shell ray tangent to ',
     $         'radius #')
        NC = 118
      end if
C     !END
      call BYE ('ANDRIA')
C
      return
      end
