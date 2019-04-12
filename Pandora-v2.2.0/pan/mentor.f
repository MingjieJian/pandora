      subroutine MENTOR
     $(IA,DX,FX,GX,N,LABEL)
C
C     Rudolf Loeser, 1983 Sep 27
C---- Dumps, for NESTOR.
C     !DASH
      save
C     !DASH
      real*8 DX, FX, GX
      integer IA, LUEO, N
      character LABEL*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, VECOUT, HI, BYE
C
C               DX(N), FX(N), GX(N)
      dimension DX(*), FX(*), GX(*)
C
      call HI ('MENTOR')
C     !BEG
        if(IA.gt.0) then
          call LINER  (3, LUEO)
          write (LUEO,100) IA,LABEL
  100     format(' ','Debug output for emergent intensity calculation',
     $               10X,'(',I8,')'/
     $           ' ',A)
C
          call VECOUT (LUEO, DX, N, 'X' )
        end if
C
        call VECOUT   (LUEO, DX, N, 'DX')
        call VECOUT   (LUEO, FX, N, 'FX')
        call VECOUT   (LUEO, GX, N, 'GX')
C     !END
      call BYE ('MENTOR')
C
      return
      end
