      subroutine RAMIREZ
     $(I,KODE,X,OPAC,TAU,N)
C
C     Rudolf Loeser, 1983 Feb 28
C---- Dumps, for Optical Depth Calculation along Rays.
C     (This is version 3 of RAMIREZ.)
C     !DASH
      save
C     !DASH
      real*8 OPAC, TAU, X
      integer I, KODE, LUEO, N
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, PRIVET, VECOUT, HI, BYE
C
C               X(N), OPAC(N), TAU(N)
      dimension X(*), OPAC(*), TAU(*)
C
      call HI ('RAMIREZ')
C     !BEG
      call LINER    (1, LUEO)
      write (LUEO,100) I
  100 format(' ','OPAC - Total Opacity for I =',I4)
      call PRIVET   (LUEO, OPAC, N)
C
      if(KODE.gt.0) then
        call VECOUT (LUEO, X   , N, 'X - Geometrical Depth')
      end if
C
      call VECOUT   (LUEO, TAU , N, 'TAU - Optical Depth'  )
C     !END
      call BYE ('RAMIREZ')
C
      return
      end
