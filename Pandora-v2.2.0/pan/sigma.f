      subroutine SIGMA
     $(N,Z,F,FDL,LABEL,W,IW)
C
C     Rudolf Loeser, 1989 Sep 14
C---- Given the tabulated function F(Z) (of length N),
C     this routine computes its logarithmic derivative, FDL.
C     (This is version 3 of SIGMA.)
C     !DASH
      save
C     !DASH
      real*8 F, FDL, W, Z
      integer IN, IS, IVEC, IW, LLT, LUEO, MOX, N
      character LABEL*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external PACIFIC, MESHED, VECOUT, ABORT, MINUSD, WGIVE, LINDA,
     $         CLIO, HI, BYE
C
      dimension W(*), IW(*)
C
C               Z(N), F(N), FDL(N)
      dimension Z(*), F(*), FDL(*)
C
      dimension IN(1)
      equivalence
     $(IN( 1),IVEC  )
C     !EJECT
C
      call HI ('SIGMA')
C     !BEG
      call MINUSD    (F, 1, N, LLT)
C
      if(LLT.gt.0) then
        call MESHED  ('SIGMA', 1)
        write (LUEO,100) LABEL
  100   format(' ','Error in computing ',A,', a logarithmic ',
     $             'derivative for the diffusion calculations.')
        call VECOUT  (LUEO, F, N, 'F')
        call ABORT
      else
C
C       (Get W allotment)
        call LINDA   (IN, IS, MOX, 'SIGMA', N)
C
        call CLIO    (F, N, W(IVEC))
        call PACIFIC (Z, W(IVEC), FDL, N, W, IW)
C
C       (Give back W allotment)
        call WGIVE   (W, 'SIGMA')
      end if
C     !END
      call BYE ('SIGMA')
C
      return
      end
