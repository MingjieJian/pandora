      subroutine ARIKI
     $(IND,KODE,ICE,ILFLX,ONAME,BC,TNU,XJNU,SIG,T1,T2,T3,WN,PHIW,CWT,
     $ PHI,XKPC,ALL,XILL,WH,WWT)
C
C     Rudolf Loeser, 1981 Dec 08
C---- Prints contents of an Orion Data Block.
C     !DASH
      save
C     !DASH
      real*8 ALL, BC, CWT, ONAME, PHI, PHIW, SIG, T1, T2, T3, TNU, WH,
     $       WN, WWT, XILL, XJNU, XKPC
      integer ICE, ILFLX, IND, IRAY, KODE, LLXI, LUEO, NRAY
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- ARCHER      as of 2004 May 12
      integer     NNKOD, NNKODS
      parameter   (NNKOD=3)
C     (Be sure to recompile POD when changing NNKOD.)
      dimension   NNKODS(NNKOD)
      common      /ARCHER/ NNKODS
C     Diana/Orion Data Blocks control parameters.
C
C     (These parameters are packed and unpacked by "POD".)
C       LLXI - frequency index.
C       IRAY - angle or ray index (Orion only: expanding atmosphere);
C              (in the spherical case, Shell rays are enumerated first,
C              followed by Disk rays).
C       NRAY - number of depth levels intersected by this ray;
C              (differs from N only for Shell rays).
C     .
      equivalence
     $(NNKODS( 1),LLXI  ),(NNKODS( 2),IRAY  ),(NNKODS( 3),NRAY  )
C     .
C     !DASH
      external POD, LINER, DASHER, VECOUT, ARROUT, HI, BYE
C
C               PHI(NRAY), XKPC(NRAY), TNU(NRAY), BC(NRAY), XJNU(NRAY),
      dimension PHI(*),    XKPC(*),    TNU(*),    BC(*),    XJNU(*),
C
C               CWT(NRAY), T1(NRAY), T2(NRAY), T3(NRAY), WH(NRAY,NRAY),
     $          CWT(*),    T1(*),    T2(*),    T3(*),    WH(*),
C
C               WN(NRAY,NRAY), PHIW(NRAY,NRAY), SIG(NRAY), WWT(NRAY)
     $          WN(*),         PHIW(*),         SIG(*),    WWT(*)
C     !EJECT
C
      call HI ('ARIKI')
C     !BEG
      call POD         (2, ONAME)
      call LINER       (2, LUEO)
      call DASHER      (LUEO)
      write (LUEO,100) IND,KODE,ICE,LLXI,IRAY,NRAY,ALL,XILL
  100 format(' ','Contents of Orion Data Block #',I5/
     $       ' ','KODE',I3,5X,'ICE',I2,5X,'LLXI',I5,5X,'IRAY',I5,5X,
     $           'NRAY',I5,5X,'ALL',1PE12.4,5X,'XILL',E12.4)
C
      if(KODE.eq.0) then
        call VECOUT   (LUEO, PHI , NRAY,       'PHI' )
        call VECOUT   (LUEO, XKPC, NRAY,       'KPC' )
        call VECOUT   (LUEO, TNU , NRAY,       'TNU' )
        call VECOUT   (LUEO, BC  , NRAY,       'BC'  )
        call VECOUT   (LUEO, CWT , NRAY,       'C'   )
        call VECOUT   (LUEO, WWT , NRAY,       'W'   )
C
        call ARROUT   (LUEO, PHIW, NRAY, NRAY, 'PHIW')
        call ARROUT   (LUEO, WN  , NRAY, NRAY, 'WN'  )
        if(ILFLX.gt.0) then
          call ARROUT (LUEO, WH  , NRAY, NRAY, 'WH'  )
        end if
C
        if(ICE.ne.0) then
          call VECOUT (LUEO, XJNU, NRAY,       'JNU' )
          call VECOUT (LUEO, SIG , NRAY,       'SIG' )
          call VECOUT (LUEO, T1  , NRAY,       'T1'  )
          call VECOUT (LUEO, T2  , NRAY,       'T2'  )
          call VECOUT (LUEO, T3  , NRAY,       'T3'  )
        end if
      end if
C     !END
      call BYE ('ARIKI')
C
      return
      end
