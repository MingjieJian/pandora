      subroutine BARBET
     $(LUMR,CP,FCP,CII,FCII,RRCP,MRJ,FRRCP,WRAT,FWRAT,P,FP,XNU,FXNU,
     $ XNUK,FNUK)
C
C     Rudolf Loeser, 1992 Jan 13
C---- Save atomic level data for ANATINI.
C     (This is version 2 of BARBET.)
C     !DASH
      save
C     !DASH
      real*8 CII, CP, P, RRCP, WRAT, XNU, XNUK
      integer IB, J, LUMR, MODE, MRJ, MRP, NSL, NTE, jummy
      logical FCII, FCP, FNUK, FP, FRRCP, FWRAT, FXNU
      character LAB*13
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(40),NSL)
      equivalence (JZQ(20),NTE)
C     !DASH
      external PUNT, MINK, HI, BYE
C
C               CP(NSL+1), CII(NTE,NSL+1), RRCP(MRS), XNU(NSL), P(NSL),
      dimension CP(*),     CII(NTE,*),     RRCP(*),   XNU(*),   P(*),
C
C               MRJ(NSL+1), WRAT(MRS)
     $          MRJ(*),     WRAT(*)
C
      data MODE /1/
C     !EJECT
C
      call HI ('BARBET')
C     !BEG
      if(FNUK) then
        call PUNT       (LUMR, XNUK    , 1  , MODE, 'NUK')
      end if
      if(FXNU) then
        call PUNT       (LUMR, XNU     , NSL, MODE, 'NU')
      end if
      if(FP) then
        call PUNT       (LUMR, P       , NSL, MODE, 'P')
      end if
      if(FCP) then
        call PUNT       (LUMR, CP      , NSL, MODE, 'CP')
      end if
C
      if(FCII.or.FRRCP.or.FWRAT) then
        do 101 J = 1,NSL
C
          write (LAB,100) J
  100     format(7X,I3,3X)
C
          if(FCII) then
            call PUNT   (LUMR, CII(1,J), NTE, MODE, 'CI  '//LAB)
          end if
C
          if(FRRCP.or.FWRAT) then
            call MINK (J, MRJ, jummy, IB)
            MRP = MRJ(J)+1
            if(FWRAT) then
              call PUNT (LUMR, WRAT(IB), MRP, MODE, 'WRAT'//LAB)
            end if
            if(FRRCP) then
              call PUNT (LUMR, RRCP(IB), MRP, MODE, 'RRCP'//LAB)
            end if
          end if
C
  101   continue
      end if
C     !END
      call BYE ('BARBET')
C
      return
      end
