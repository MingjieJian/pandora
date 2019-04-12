      subroutine DAPHNIS
     $(DUMP,KASE,I,INT,WVL,KTIT,TIT,KILROY,LABEL,DMPI)
C
C     Rudolf Loeser, 2004 Feb 19
C---- Sets up dump paraphernalia for continuous emergent intensity
C     calculations.
C     (This is version 2 of DAPHNIS.)
C     !DASH
      save
C     !DASH
      real*8 WVL
      integer I, INT, KASE, KTIT, NC
      logical DMPI, DUMP, KILROY
      character LABEL*(*), TIT*(*)
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external ANDRIA, HALT, HI, BYE
C
      call HI ('DAPHNIS')
C     !BEG
      DMPI = DUMP.and.(I.eq.INT)
C
      if(KASE.eq.1) then
        write (LABEL,100) TIT,WVL,I
  100   format('Continuum Intensity for ',A,' at',1PE20.12,
     $         ' Angstroms, Disk Ray #',I4)
      else if(KASE.eq.2) then
        if(KILROY) then
          KILROY = .false.
          call ANDRIA (KTIT, WVL, LABEL, NC)
        end if
        write (LABEL(NC+1:NC+4),101) I
  101   format(I4)
C
      else
        write (MSSLIN(1),102) KASE
  102   format('KASE =',I12,', which is not 1 or 2.')
        call HALT     ('DAPHNIS', 1)
      end if
C     !END
      call BYE ('DAPHNIS')
C
      return
      end
