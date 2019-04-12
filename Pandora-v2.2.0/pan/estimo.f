      subroutine ESTIMO
     $(N,RMASS,HN1,TE,FC,KILROY)
C
C     Rudolf Loeser, 1991 May 07
C---- Computes FC for DAMALA.
C     !DASH
      save
C     !DASH
      real*8 FAC, FC, HN1, RMASS, RT, TE
      integer I, N
      logical KILROY
C     !DASH
      external HI, BYE
C
C               HN1(N), TE(N), FC(N)
      dimension HN1(*), TE(*), FC(*)
C
      data FAC /1.055D-2/
C
      call HI ('ESTIMO')
C     !BEG
      if(KILROY) then
        KILROY = .false.
        do 100 I = 1,N
          RT    = sqrt(TE(I))
          FC(I) = ((FAC*RMASS)*HN1(I))/RT
  100   continue
      end if
C     !END
      call BYE ('ESTIMO')
C
      return
      end
