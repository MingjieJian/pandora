      subroutine SHABBY
     $(LU,SHCOP,SHCOC,ZRCO,CTMX,CTCO,N,TE,FCT,TCO)
C
C     Rudolf Loeser, 2006 Dec 29
C---- Prints, for HAMRU.
C     !DASH
      save
C     !DASH
      real*8 CTCO, CTMX, FCT, SHCOC, SHCOP, TCO, TE, ZRCO
      integer LU, N
C     !DASH
      external LINER, DASHER, VECOUT, HI, BYE
C
C               FCT(N), TCO(N), TE(N)
      dimension FCT(*), TCO(*), TE(*)
C
      call HI ('SHABBY')
C     !BEG
      if(LU.gt.0) then
        call LINER  (2, LU)
        call DASHER (LU)
        call LINER  (1, LU)
        write (LU,100) SHCOP,SHCOC,ZRCO,CTMX,CTCO
  100   format(' ','Computation of TCO, the carbon monoxide ',
     $             'formation temperature.',1P//
     $         ' ','SHCOP =',E12.4,10X,'SHCOC =',E12.4,10X,
     $             'ZRCO =',E12.4/
     $         ' ',' CTMX =',E12.4,10X,' CTCO =',E12.4)
        call VECOUT (LU, TE,  N, 'TE' )
        call VECOUT (LU, FCT, N, 'FCT')
        call VECOUT (LU, TCO, N, 'TCO')
        call LINER  (1, LU)
        call DASHER (LU)
        call LINER  (1, LU)
      end if
C     !END
      call BYE ('SHABBY')
C
      return
      end
