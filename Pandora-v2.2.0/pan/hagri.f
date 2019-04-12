      subroutine HAGRI
     $(IQHSE,IQCPS,CPRSS,QNAME,QELSM,KPRSW)
C
C     Rudolf Loeser, 1984 Oct 17
C---- Sets constant-pressure NH-adjustment switch.
C     !DASH
      save
C     !DASH
      real*8 CPRSS, ZERO
      integer IQCPS, IQHSE, KPRSW
      character QELSM*8, QNAME*8
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external HI, BYE
C
      call HI ('HAGRI')
C     !BEG
      KPRSW = 0
      if((IQHSE.le.0).and.(IQCPS.gt.0)) then
        if(CPRSS.gt.ZERO) then
          if((QNAME.eq.'HYDROGEN').or.(QELSM.eq.'H       ')) then
            KPRSW = 1
          end if
        end if
      end if
C     !END
      call BYE ('HAGRI')
C
      return
      end
