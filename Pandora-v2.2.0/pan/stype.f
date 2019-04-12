      subroutine STYPE
     $(XNUU,XNUL,CSK)
C
C     Rudolf Loeser, 2005 Feb 23
C---- Computes the default value of CSK(u,l).
C     !DASH
      save
C     !DASH
      real*8 CSK, FAC, FEFF, XNUL, XNUU
C     !DASH
      external KNAVE, HI, BYE
C
      data FAC /2.386D-9/
C
      call HI ('STYPE')
C     !BEG
      call KNAVE (XNUU, FEFF)
      CSK = FAC*((FEFF**5)/((XNUU-XNUL)**2))
C     !END
      call BYE ('STYPE')
C
      return
      end
