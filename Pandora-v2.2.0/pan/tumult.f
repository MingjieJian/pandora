      subroutine TUMULT
     $(W,N,NM,X,IM,IP)
C
C     Rudolf Loeser, 2003 Jan 09
C---- Finds wavelength indices for MULLET.
C     !DASH
      save
C     !DASH
      real*8 DELTA, W, X
      integer IM, IP, K, LOOK, LUEO, N, NM, NOTE
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LOOKSD, MESHED, ABORT, HI, BYE
C
C               W(N)
      dimension W(*)
C
      data DELTA /0.D0/
C
      call HI ('TUMULT')
C     !BEG
      call LOOKSD   (W,N,DELTA,X,K,NOTE,LOOK)
C
      if(LOOK.ne.1) then
        call MESHED ('TUMULT',1)
        write (LUEO,100) X,LOOK,W(1),N,W(N)
  100   format(' ','Error placing XLM in precomputed wavelengths ',
     $             'table for highest H Ly lines.'/
     $         ' ','XLM =',1PE20.12,'; LOOK =',I12,', which is not 1.'/
     $         ' ','WN(1) =',E20.12,5X,'WN(',I3,') =',E20.12)
        call ABORT
      end if
C
      if(K.gt.NM) then
        IM = NM
        IP = 0
      else
        IM = K
        IP = K+1
      end if
C     !END
      call BYE ('TUMULT')
C
      return
      end
