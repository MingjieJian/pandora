      subroutine POLLY
     $(JLEV,N,MTR,TR,CP,XNU,RK,TREFF,TRI,RKI,RKR)
C
C     Rudolf Loeser, 1984 Apr 11
C---- Computes TREFF for level JLEV.
C     !DASH
      save
C     !DASH
      real*8 CP, RK, RKI, RKR, TR, TREFF, TRI, TRMN, TRMX, XNU
      integer I, JLEV, MTR, N
      logical DMPD, DMPE, HEAD, SOME
C     !DASH
      external ADDER, LUGGAGE, NOBBY, MASHED, HI, BYE
C
C               TREFF(N), TR(N,MTR), CP(NSL+1), XNU(NSL), TRI(MTR),
      dimension TREFF(*), TR(*),     CP(*),     XNU(*),   TRI(*),
C
C               RK(N), RKI(MTR), RKR(MTR)
     $          RK(*), RKI(*),   RKR(*)
C
      call HI ('POLLY')
C     !BEG
      HEAD = .false.
      do 100 I = 1,N
        call NOBBY     (I, DMPD, DMPE, HEAD, JLEV, 'POLLY')
        call ADDER     (JLEV, I, N, MTR, TR, XNU, CP, RK(I), TRI, RKR,
     $                  RKI, TRMN, TRMX, SOME, DMPE)
        if(SOME) then
          call LUGGAGE (I, JLEV, XNU, CP, RK(I), TRMN, TRMX, TREFF(I),
     $                  DMPD, DMPE)
        end if
  100 continue
      if(HEAD) then
        call MASHED ('POLLY')
      end if
C     !END
      call BYE ('POLLY')
C
      return
      end
