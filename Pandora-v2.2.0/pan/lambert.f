      subroutine LAMBERT
     $(LU,N,KION,SHE,ADD)
C
C     Rudolf Loeser, 1998 Mar 06
C---- Prints supplementary diffusion results.
C     (This is version 2 of LAMBERT.)
C     !DASH
      save
C     !DASH
      real*8 ADD, SHE
      integer KION, LU, N
C     !DASH
      external LINER, PRIVET, HI, BYE
C
C               SHE(N), ADD(N)
      dimension SHE(*), ADD(*)
C
      call HI ('LAMBERT')
C     !BEG
      if((LU.gt.0).and.(KION.eq.3)) then
        call LINER  (2,LU)
        write (LU,100)
  100   format(' ','Neutral Helium ( = SHE = sum of Helium-I level ',
     $             'populations)')
        call PRIVET (LU,SHE,N)
C
        call LINER  (2,LU)
        write (LU,101)
  101   format(' ','"Additional term" ( = ADD), for Helium-II')
        call PRIVET (LU,ADD,N)
      end if
C     !END
      call BYE ('LAMBERT')
C
      return
      end
