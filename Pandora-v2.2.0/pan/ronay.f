      subroutine RONAY
     $(PL,N,NP,SIG, IBEG)
C
C     Rudolf Loeser, 1996 Mar 05
C---- Finds the starting index, for SKYE.
C     !DASH
      save
C     !DASH
      real*8 PL, SIG
      integer I, IBEG, J, N, NP
C     !DASH
      external HI, BYE
C
C               PL(N,NP)
      dimension PL(N,*)
C
      call HI ('RONAY')
C     !BEG
      IBEG = 0
C
      do 101 I = 1,N
        do 100 J = 1,NP
          if(PL(I,J).ne.SIG) then
            IBEG = I
            goto 102
          end if
  100   continue
  101 continue
C
  102 continue
C     !END
      call BYE ('RONAY')
C
      return
      end
