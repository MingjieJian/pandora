      subroutine HAW
     $(XMU,CMU,WMU,LFLX,LG)
C
C     Rudolf Loeser, 1981 Apr 23
C---- Computes mu-integration weights.
C     (This is version 3 of HAW.)
C     !DASH
      save
C     !DASH
      real*8 CMU, HALF, WMU, XMU
      integer I, LFLX, LG, LM
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(12),HALF  )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external HALT, ARRMUL, HI, BYE
C
C               XMU(LG), CMU(LG), WMU(LG)
      dimension XMU(*),  CMU(*),  WMU(*)
C     !EJECT
C
      call HI ('HAW')
C     !BEG
      if(LG.lt.1) then
        write (MSSLIN(1),100) LG
  100   format('LG =',I12,', which is less than 1.')
        call HALT ('HAW', 1)
C
      else if(LG.eq.1) then
        CMU(1) = XMU(1)
      else
C
        LM = LG-1
        CMU(1)    = HALF*(XMU( 1)-XMU( 2))
        CMU(LG)   = HALF*(XMU(LM)+XMU(LG))
C
        if(LM.ge.2) then
          do 101 I = 2,LM
            CMU(I) = HALF*(XMU(I-1)-XMU(I+1))
  101     continue
        end if
      end if
C
      if(LFLX.gt.0) then
        call ARRMUL (XMU, CMU, WMU, LG)
      end if
C     !END
      call BYE ('HAW')
C
      return
      end
