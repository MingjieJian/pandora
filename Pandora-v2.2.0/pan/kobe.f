      subroutine KOBE
     $(MSFT,BOTH,N,IDDL,KK,EDITS,BA,BF)
C
C     Rudolf Loeser, 2004 May 07
C---- Miscellaneous housekeeping for DIRECT solutions.
C     (This is version 3 of KOBE.)
C     !DASH
      save
C     !DASH
      real*8 BA, BF
      integer IDDL, KK, M, MSFT, N
      logical BOTH, EDITS
C     !DASH
      external ZERO1, HI, BYE
C
C               BA(N), BF(N)
      dimension BA(*), BF(*)
C
      call HI ('KOBE')
C     !BEG
      if(MSFT.eq.1) then
        call ZERO1   (BA, N)
        call ZERO1   (BF, N)
        IDDL  = N
        EDITS = .false.
      else
        if(BOTH) then
          M = (N-KK)+1
          call ZERO1 (BA(KK), M)
          call ZERO1 (BF(KK), M)
        end if
      end if
C     !END
      call BYE ('KOBE')
C
      return
      end
