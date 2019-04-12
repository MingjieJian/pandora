      subroutine TROCAR
     $(KODE,FO,FN,FRAT,N,LABEL,W,IW,DUMP)
C
C     Rudolf Loeser, 1999 Jan 14
C---- Makes FN, a dampened version of FO.
C     !DASH
      save
C     !DASH
      real*8 FN, FO, FRAT, W
      integer IW, KODE, LUEO, N
      logical DUMP
      character LABEL*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external M3AV, ARRDIV, TARCO, ONE1, MOVE1, CROAT, LINER, HI, BYE
C
      dimension W(*), IW(*)
C
C               FO(N), FN(N), FRAT(N)
      dimension FO(*), FN(*), FRAT(*)
C
      call HI ('TROCAR')
C     !BEG
      call ONE1       (FRAT, N)
C
      if((KODE.gt.0).or.DUMP) then
        call M3AV     (FO, FN, N)
        if(DUMP) then
          call TARCO  (FO, FN, FRAT, N, KODE, LABEL)
        else
          call ARRDIV (FN, FO, FRAT, N)
        end if
        if(KODE.gt.0) then
          call MOVE1  (FN, N, FO)
        end if
        if(DUMP) then
          call CROAT  (FRAT, FN, N, LABEL)
C
          call LINER  (3, LUEO)
          write (LUEO,100) LABEL
  100     format(' ','***** End of damping of ',A)
C
          if(KODE.le.0) then
            call ONE1 (FRAT, N)
          end if
        end if
      end if
C     !END
      call BYE ('TROCAR')
C
      return
      end
