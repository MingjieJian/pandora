      subroutine ARUNTA
     $(I,KODE,ICE,ILFLX,N,PHI,BC,TNU,XJNU,SIG,T1,T2,T3,WN,PHIW,XKPC,WH,
     $ DNAME,ALL,XILL)
C
C     Rudolf Loeser, 1981 Dec 08
C---- Prints contents of a Diana Data Block.
C     !DASH
      save
C     !DASH
      real*8 ALL, BC, DNAME, PHI, PHIW, SIG, T1, T2, T3, TNU, WH, WN,
     $       XILL, XJNU, XKPC
      integer I, ICE, ILFLX, KODE, LLXI, LUEO, N
C     !COM
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
      equivalence (NNKODS( 1),LLXI  )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external POD, LINER, DASHER, VECOUT, ARROUT, HI, BYE
C
C               PHI(N), XKPC(N), PHIW(N,N), XJNU(N), WN(N,N), WH(N,N),
      dimension PHI(*), XKPC(*), PHIW(*),   XJNU(*), WN(*),   WH(*),
C
C               BC(N), TNU(N), SIG(N), T1(N), T2(N), T3(N)
     $          BC(*), TNU(*), SIG(*), T1(*), T2(*), T3(*)
C     !EJECT
C
      call HI ('ARUNTA')
C     !BEG
      call POD        (2, DNAME)
      call LINER      (2, LUEO)
      call DASHER     (LUEO)
      write (LUEO,100) I,KODE,ICE,N,LLXI,ALL,XILL
  100 format(' ','Contents of Diana Data Block #',I3/
     $       ' ','KODE',I2,5X,'ICE',I2,5X,'N',I4,5X,'LLXI',I4,5X,'ALL',
     $           1PE12.4,5X,'XILL',E12.4)
C
      if(KODE.eq.0) then
        call VECOUT   (LUEO, PHI,  N,    'PHI' )
        call VECOUT   (LUEO, BC,   N,    'BC'  )
        call VECOUT   (LUEO, XKPC, N,    'KPC' )
        call VECOUT   (LUEO, TNU,  N,    'TNU' )
C
        call ARROUT   (LUEO, PHIW, N, N, 'PHIW')
        call ARROUT   (LUEO, WN,   N, N, 'WN'  )
        if(ILFLX.gt.0) then
          call ARROUT (LUEO, WH,   N, N, 'WH'  )
        end if
C
        if(ICE.ne.0) then
          call VECOUT (LUEO, XJNU, N,    'JNU' )
          call VECOUT (LUEO, SIG,  N,    'SIG' )
          call VECOUT (LUEO, T1,   N,    'T1'  )
          call VECOUT (LUEO, T2,   N,    'T2'  )
          call VECOUT (LUEO, T3,   N,    'T3'  )
        end if
      end if
C     !END
      call BYE ('ARUNTA')
C
      return
      end
