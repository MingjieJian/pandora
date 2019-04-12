      subroutine ERWIN
     $(HND,XNE,DEP)
C
C     Rudolf Loeser, 1978 Sep 07
C---- Computes density term, for RAGOUT.
C     !DASH
      save
C     !DASH
      real*8 DEP, FIFTH, FNH, HND, ONE, P, XNE, ZERO
C     !COM
C---- DICOM       as of 2000 Mar 01
      integer     NAPWRA,NAPWRB,NAPKNT
      real*8      APARAD,APETA,APCDP,APWRA,APWRB,APCI,APEI
      dimension   APCI(20), APEI(20)
      common      /DICOM1/ NAPWRA,NAPWRB,NAPKNT
      common      /DICOM2/ APARAD,APETA,APCDP,APWRA,APWRB,APCI,APEI
C     Data for recombinations.
C     .
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(15),FIFTH )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external WINER, DIVIDE, HALT, HI, BYE
C
      call HI ('ERWIN')
C     !BEG
      if(XNE.le.ZERO) then
        write (MSSLIN(1),100) XNE
  100   format('NE =',1PE24.16,'; must be greater than zero (for ',
     $         'density term in dielectronic recombination.')
        call HALT ('ERWIN', 1)
      end if
C
      P = XNE**FIFTH
      call WINER  (HND, FNH)
      call DIVIDE (FNH, (ONE+APCDP*P), DEP)
C     !END
      call BYE ('ERWIN')
C
      return
      end
