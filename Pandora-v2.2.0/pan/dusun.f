      subroutine DUSUN
     $(W,N,WA,NA)
C
C     Rudolf Loeser, 1983 Mar 14
C---- Augments the order of a weight matrix by one.
C     Note: NA = N+1.
C     (This is version 2 of DUSUN.)
C     !DASH
      save
C     !DASH
      real*8 HALF, W, WA
      integer I, IA, J, JA, N, NA
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(12),HALF  )
C     !DASH
      external HI, BYE
C
C               W(N,N), WA(NA,NA)
      dimension W(N,*), WA(NA,*)
C
      call HI ('DUSUN')
C     !BEG
      do 101 IA = 1,NA
C
        if(IA.lt.N) then
          I = IA
        else
          I = IA-1
        end if
C
        do 100 JA = 1,NA
C
          if(JA.lt.N) then
            J = JA
          else
            J = JA-1
          end if
C
          if((JA.eq.(N-1)).or.(JA.eq.N)) then
            WA(IA,JA) = HALF*W(I,J)
          else
            WA(IA,JA) = W(I,J)
          end if
C
  100   continue
  101 continue
C     !END
      call BYE ('DUSUN')
C
      return
      end
