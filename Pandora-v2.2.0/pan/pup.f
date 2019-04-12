      subroutine PUP
     $(LU,I,ABDC,ABDO,HND,TE,TCO,ECO,ECH,EOH,CND,OND,CON,CHN,OHN)
C
C     Rudolf Loeser, 2006 Dec 28
C---- Prints details of molecular number densities calculation.
C     (This is version 4 of PUP.)
C     !DASH
      save
C     !DASH
      real*8 ABDC, ABDO, CHN, CND, CON, ECH, ECO, EOH, HND, OHN, OND,
     $       TCO, TE
      integer I, LU
C     !DASH
      external BURDOCK, SHIM, HI, BYE
C
      call HI ('PUP')
C     !BEG
      if(LU.gt.0) then
C
        if(I.eq.1) then
C----     Provide heading
          call BURDOCK (LU, ABDC, ABDO)
        end if
C
        write (LU,100) I,HND,TE,TCO,ECO,ECH,EOH,CND,OND,CON,CHN,OHN
  100   format(' ',I5,1P11E11.3)
        call SHIM      (I, 5, LU)
C
      end if
C     !END
      call BYE ('PUP')
C
      return
      end
