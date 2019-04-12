      subroutine POD
     $(MODE,XNAME)
C
C     Rudolf Loeser, 1982 Nov 22
C---- Encodes (MODE=1) or decodes (MODE=2)
C     a DIANA/ORION Data Block type specification.
C     !DASH
      save
C     !DASH
      real*8 FACTS, XNAME
      integer MODE
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
C     !DASH
      external GENISTA, HI, BYE
C
      dimension FACTS(NNKOD)
C
      data FACTS /1.D4, 1.D3, 1.D3/
C
      call HI ('POD')
C     !BEG
      call GENISTA (MODE, XNAME, NNKODS, FACTS, NNKOD, 'POD')
C     !END
      call BYE ('POD')
C
      return
      end
