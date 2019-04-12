      subroutine HEMRA
     $(NSW,SCOW,IPNT)
C
C     Rudolf Loeser, 2002 Aug 21
C---- Sorts Selected Continuum Output Wavelengths into ascending order.
C     !DASH
      save
C     !DASH
      real*8 SCOW
      integer IPNT, NSW
C     !DASH
      external SORT, HI, BYE
C
C               SCOW(NSW), IPNT(NSW)
      dimension SCOW(*),   IPNT(*)
C
      call HI ('HEMRA')
C     !BEG
      if(NSW.gt.1) then
        call SORT (SCOW, NSW, IPNT,
     $             'Selected Continuum Output Wavelengths (SCOW)')
      end if
C     !END
      call BYE ('HEMRA')
C
      return
      end
