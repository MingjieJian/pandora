      subroutine SLOUGH
     $(IWS,N,IU,MU,JU,CU,ID,MD,JD,CD)
C
C     Rudolf Loeser, 1978 Jul 26
C---- Scans up and down, for TAIFUN.
C     !DASH
      save
C     !DASH
      real*8 CD, CU, ZERO
      integer ID, IU, IWS, JD, JU, MD, MU, N
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
C               IWS(N)
      dimension IWS(*)
C
      call HI ('SLOUGH')
C     !BEG
C---- Scan up
      if((JU.gt.0).and.(MU.gt.0)) then
        IU = IU+1
        if(IU.le.N) then
          CU = IWS(IU)
        else
          CU = ZERO
          MU = 0
          IU = N
        end if
      end if
C---- Scan down
      if((JD.gt.0).and.(MD.gt.0)) then
        ID = ID-1
        if(ID.gt.0) then
          CD = IWS(ID)
        else
          CD = ZERO
          MD = 0
          ID = 1
        end if
      end if
C     !END
      call BYE ('SLOUGH')
C
      return
      end
